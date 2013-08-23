defmodule Mana.Connection do
  use GenServer.Behaviour

  alias Data.Dict
  alias Mana.Server

  defrecord State, connections: nil, options: nil
  defrecord Connection, server: nil, socket: nil, reader: nil, writer: nil

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  def init(options) do
    Process.flag(:trap_exit, true)

    { :ok, State[options: options, connections: []] }
  end

  defp connect(Server[host: host, port: port, secure: secure, password: password] = server) do
    socket = if secure do
      Socket.SSL.connect!(host, port, packet: :line, automatic: false)
    else
      Socket.TCP.connect!(host, port, packet: :line, automatic: false)
    end

    if password do
      socket |> write "PASS #{password}"
    end

    socket |> write "NICK Mana"
    socket |> write "USER Mana * * :Succubus begone."

    Connection[ server: server,
                socket: socket,
                reader: Process.spawn_link(__MODULE__, :reader, [socket]),
                writer: Process.spawn_link(__MODULE__, :writer, [socket]) ]
  end

  defp write(socket, line) do
    socket.send([line, ?\r, ?\n])
  end

  def call(what) do
    :gen_server.call(__MODULE__, what)
  end

  def send(what, options // []) do
    if to = options[:to] do
      call({ :get, :writer, to }) <- what
    else
      Enum.each call({ :all, :writer }), fn { _, pid } ->
        pid <- what
      end
    end
  end

  def handle_call({ :connect, server }, _from, State[connections: connections] = state) do
    unless Dict.has_key?(connections, server.name) do
      { :reply, :connecting, state.connections(Dict.put(connections, server.name, connect(server))) }
    else
      { :reply, :already_connected, state }
    end
  end

  def handle_call({ :join, channel }, _from, State[connections: connections] = state) do
    conn = Dict.get(connections, channel.server)
    conn = Dict.put(conn.server.channels, channel.name, channel) |> conn.server.channels |> conn.server

    conn.socket |> write "JOIN #{channel.name}"

    { :reply, :ok, Dict.put(connections, conn.server.name, conn) |> state.connections }
  end

  def handle_call({ :all, what }, _from, State[connections: connections] = state) when what in %w[writer reader]a do
    result = Enum.map connections, fn { name, Connection[reader: reader, writer: writer] } ->
      { name, case what do
        :writer -> writer
        :reader -> reader
      end }
    end

    { :reply, result, state }
  end

  def handle_call({ :get, what, name }, _from, State[connections: connections] = state) when what in %w[writer reader]a do
    if name |> is_record Server do
      name = name.name
    end

    pid = Enum.find_value connections, fn
      { ^name, Connection[reader: reader, writer: writer] } ->
        case what do
          :writer -> writer
          :reader -> reader
        end

      _ ->
        false
    end

    { :reply, pid, state }
  end

  def handle_call({ :get, :server, socket }, _from, State[connections: connections] = state) do
    server = Enum.find_value connections, fn { _, Connection[server: server, socket: ^socket] } ->
      server
    end

    { :reply, server, state }
  end

  def handle_call({ :get, :channel, server, name }, _from, State[connections: connections] = state) do
    if server |> is_record Server do
      server = server.name
    end

    { :reply, connections |> Dict.get(server) |> Connection.server |> Server.channels |> Dict.get(name), state }
  end

  def handle_info({ :EXIT, pid, reason }, _from, State[connections: connections] = state) do
    if reason == :reconnect do
      { :noreply, state }
    else
      { server, conn } = Enum.find connections, fn
        { _, Connection[reader: ^pid] } -> true
        { _, Connection[writer: ^pid] } -> true
        _                               -> false
      end

      connections = if pid == conn.reader do
        Process.exit conn.writer, :reconnect
        conn.socket.close

        Dict.put(connections, server, connect(conn.server))
      else
        Process.exit conn.reader, :reconnect

        Dict.put(connections, server,
          conn.writer(Process.spawn_link(__MODULE__, :writer, [conn.socket])))
      end

      { :noreply, state.connections(connections) }
    end
  end

  @doc false
  def reader(socket) do
    Mana.Plugin <- { :receive, call({ :get, :server, socket }), socket.recv! |> String.strip }

    reader(socket)
  end

  @doc false
  def writer(socket) do
    receive do
      line when is_binary(line) ->
        socket.send!([line, ?\r, ?\n])
    end

    writer(socket)
  end
end

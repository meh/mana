defmodule Mana.Connection do
  use GenServer.Behaviour

  alias Data.Dict
  alias Mana.Server

  defrecord State, connections: nil, options: nil
  defrecord Connection, server: nil, socket: nil

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  def init(options) do
    Process.flag(:trap_exit, true)

    { :ok, State[options: options, connections: []] }
  end

  defp connect(Server[host: host, port: port, secure: secure, nick: nick, user: user, realname: realname, password: password] = server) do
    socket = if secure do
      Socket.SSL.connect!(host, port, packet: :line, mode: :active)
    else
      Socket.TCP.connect!(host, port, packet: :line, mode: :active)
    end

    if password do
      socket |> write "PASS #{password}"
    end

    socket |> write "NICK #{nick}"
    socket |> write "USER #{user} * * :#{realname}"

    Connection[server: server, socket: socket]
  end

  defp write(socket, line) do
    socket |> Socket.Stream.send! [line, ?\r, ?\n]
  end

  def call(what) do
    :gen_server.call(__MODULE__, what)
  end

  def send(what, options // []) do
    if to = options[:to] do
      call { :write, to, what }
    else
      call { :write, :all, what }
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

    { :reply, :ok, Dict.put(connections, conn.server.name, conn) |> state.connections }
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

  def handle_call({ :write, :all, message }, _from, State[connections: connections] = _state) do
    Enum.each connections, fn { _name, Connection[socket: socket] } ->
      socket |> Socket.Stream.send! [message, "\r\n"]
    end

    { :reply, :ok, _state }
  end

  def handle_call({ :write, name, message }, _from, State[connections: connections] = _state) do
    if name |> is_record Server do
      name = name.name
    end

    Dict.get(connections, name).socket |> Socket.Stream.send! [message, "\r\n"]

    { :reply, :ok, _state }
  end

  def handle_info({ :tcp, socket, line }, State[connections: connections] = _state) do
    Mana.Plugin.cast { :receive, server_for(connections, socket), line |> String.rstrip }

    { :noreply, _state }
  end

  def handle_info({ :tcp_closed, socket }, State[connections: connections] = state) do
    server = server_for(connections, socket)

    { :noreply, state.connections(connections |> Dict.put(server.name, connect(server))) }
  end

  def handle_info({ :tcp_error, socket, _reason }, State[connections: connections] = state) do
    server = server_for(connections, socket)

    { :noreply, state.connections(connections |> Dict.put(server.name, connect(server))) }
  end

  defp server_for(connections, socket) do
    Enum.find_value connections, fn { _name, Connection[server: server, socket: sock] } ->
      if Socket.equal?(socket, sock) do
        server
      end
    end
  end
end

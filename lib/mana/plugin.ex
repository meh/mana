defmodule Mana.Plugin do
  use Behaviour

  defcallback handle(record, term) :: { :ok, state :: term } |
                                      { :error, reason :: term, state :: term }

  defcallback call(term, term) :: { :ok, result :: term, state :: term } |
                                  { :error, reason :: term, state :: term }

  defmacro __using__(_opts) do
    quote do
      alias Mana.Event
      alias Mana.User
      alias Mana.Plugin

      @behaviour Mana.Plugin
      @before_compile unquote(__MODULE__)

      defmacrop defer(do: block) do
        quote do
          Process.spawn fn ->
            unquote(block)
          end
        end
      end

      def init(_options) do
        { :ok, nil }
      end

      def terminate(_reason, _state) do
        nil
      end

      defoverridable init: 1
      defoverridable terminate: 2

      use GenServer.Behaviour

      def start_link(options) do
        :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
      end

      def handle_cast({ :handle, event }, state) do
        case handle(event, state) do
          { :ok, state } ->
            { :noreply, state }

          { :error, _reason, state } ->
            { :noreply, state }
        end
      end

      def handle_cast(:terminate, state) do
        { :stop, :normal, state }
      end

      def handle_call({ :call, args }, _from, state) do
        case call(args, state) do
          { :ok, reply, state } ->
            { :reply, reply, state }

          { :error, reason, state } ->
            { :reply, { :error, reason }, state }
        end
      end

    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def handle(_, state) do
        { :ok, state }
      end

      def call(_, state) do
        { :error, :unimplemented, state }
      end
    end
  end

  use GenServer.Behaviour

  @timeout 30_000

  defrecord State, options: nil, plugins: []
  defrecord Plugin, name: nil, module: nil, pid: nil

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  def init(options) do
    Process.flag(:trap_exit, true)

    { :ok, State[options: options] }
  end

  def call(what) do
    :gen_server.call(__MODULE__, what)
  end

  def call(plugin, what) do
    :gen_server.call(__MODULE__, { :call, plugin, what }, @timeout)
  end

  def cast(what) do
    :gen_server.cast(__MODULE__, what)
  end

  def handle_call({ :register, name, module }, _from, State[options: options, plugins: plugins] = state) do
    if Dict.has_key?(plugins, name) do
      :gen_server.cast(module, :terminate)

      { :reply, :restarting, state }
    else
      case module.start_link(options) do
        { :ok, pid } ->
          { :reply, :ok, state.plugins(Dict.put(plugins, name,
            Plugin[name: name, module: module, pid: pid])) }

        { :error, _ } = e ->
          { :reply, e, state }
      end
    end
  end

  def handle_call({ :call, name, what }, _from, State[plugins: plugins] = state) do
    case :gen_server.call(Dict.get(plugins, name).module, { :call, what }, @timeout) do
      { :error, _ } = e ->
        { :reply, e, state }

      result ->
        { :reply, result, state }
    end
  end

  def handle_call({ :event, name, event }, _from, State[plugins: plugins] = state) do
    :gen_server.cast(Dict.get(plugins, name).module, { :handle, event })

    { :reply, :ok, state }
  end

  def handle_cast({ :receive, server, line }, State[plugins: plugins] = state) do
    case line do
      ":" <> rest ->
        [from, rest] = String.split rest, " ", global: false

        case rest do
          "JOIN :" <> channel ->
            channel = Mana.Connection.call({ :get, :channel, server, channel })
            event   = Mana.Event.Join.new(
              server: server,
              channel: channel,
              user:    Mana.User.parse(from))

            handle(plugins, channel, event)

          "PART " <> rest ->
            { channel, reason } = case rest |> String.split " ", global: false do
              [channel] ->
                { channel, nil }

              [channel, ":" <> reason] ->
                { channel, reason }
            end

            channel = Mana.Connection.call({ :get, :channel, server, channel })
            event   = Mana.Event.Leave.new(
              server:  server,
              channel: channel,
              user:    Mana.User.parse(from),
              reason:  reason)

            handle(plugins, channel, event)

          "PRIVMSG " <> rest ->
            [channel, ":" <> message] = rest |> String.split(" ", global: false)

            channel = Mana.Connection.call({ :get, :channel, server, channel })
            event   = Mana.Event.Message.new(
              server:  server,
              channel: channel,
              user:    Mana.User.parse(from),
              content: message)

            handle(plugins, channel, event)

          "422 " <> _ ->
            connected(server)

          "376 " <> _ ->
            connected(server)

          _ ->
            nil
        end

      "PING " <> rest ->
        server.send "PONG #{rest}"

      _ ->
        nil
    end

    { :noreply, state }
  end

  def handle_info({ :EXIT, pid, _reason }, State[plugins: plugins, options: options] = state) do
    plugin = Enum.find plugins, fn
      Plugin[pid: ^pid] -> true
      _                 -> false
    end

    case plugin && plugin.module.start_link(options) do
      { :ok, pid } ->
        { :noreply, state.plugins(Dict.put(plugins, plugin.pid(pid)))  }

      _ ->
        { :noreply, state }
    end
  end

  defp handle(plugins, channel, event) do
    Enum.each plugins, fn { name, Plugin[module: module] } ->
      if Data.contains?(channel.plugins, name) do
        :gen_server.cast(module, { :handle, event })
      end
    end
  end

  defp connected(server) do
    Enum.each server.channels, fn { name, _ } ->
      server.send "JOIN #{name}"
    end
  end
end

defmodule Mana.Plugin do
  use Behaviour

  defcallback handle(record, term) :: { :ok, state :: term } |
                                      { :error, reason :: term, state :: term }

  defcallback call(term, term) :: { :ok, result :: term, state :: term } |
                                  { :error, reason :: term, state :: term }

  defcallback info(term, term) :: { :ok, state :: term } |
                                  { :error, reason :: term, state :: term }

  defmacro __using__(_opts) do
    quote do
      alias Mana.Event
      alias Mana.Server
      alias Mana.Channel
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

      def handle_info(term, state) do
        case info(term, state) do
          { :ok, state } ->
            { :noreply, state }

          { :error, _reason, state } ->
            { :noreply, state }
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

      def info(_, state) do
        { :ok, state }
      end
    end
  end

  use GenServer.Behaviour

  alias Mana.Connection
  alias Mana.Event
  alias Mana.User

  @timeout 30_000

  defrecord State, options: nil, plugins: []
  defrecord Plugin, name: nil, module: nil, pid: nil

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  def init(options) do
    Process.flag :trap_exit, true

    if main = options[:main] do
      Process.spawn fn ->
        Mana.load main
      end
    end

    { :ok, State[options: options] }
  end

  def register(name, module) do
    :gen_server.call(__MODULE__, { :register, name, module }, @timeout)
  end

  def call(plugin, what, timeout // 10_000) do
    :gen_server.call(__MODULE__, { :call, plugin, what }, timeout)
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
    case Dict.get(plugins, name) do
      nil ->
        { :reply, :not_found, state }

      plugin ->
        case :gen_server.call(plugin.module, { :call, what }, @timeout) do
          { :error, _ } = e ->
            { :reply, e, state }

          result ->
            { :reply, result, state }
        end
    end
  end

  def handle_call({ :event, name, event }, _from, State[plugins: plugins] = state) do
    case Dict.get(plugins, name) do
      nil ->
        { :reply, :not_found, state }

      plugin ->
        :gen_server.cast(plugin.module, { :handle, event })

        { :reply, :ok, state }
    end
  end

  def handle_cast({ :receive, server, line }, State[plugins: plugins] = state) do
    case line do
      ":" <> rest ->
        [from, rest] = String.split rest, " ", global: false

        case rest do
          "JOIN :" <> channel ->
            channel = Connection.call({ :get, :channel, server, channel })
            event   = Event.Join.new(
              server: server,
              channel: channel,
              user:    User.parse(from))

            handle(plugins, channel, event)

          "PART " <> rest ->
            { channel, reason } = case rest |> String.split " ", global: false do
              [channel] ->
                { channel, nil }

              [channel, ":" <> reason] ->
                { channel, reason }
            end

            channel = Connection.call({ :get, :channel, server, channel })
            event   = Event.Leave.new(
              server:  server,
              channel: channel,
              user:    User.parse(from),
              reason:  reason)

            handle(plugins, channel, event)

          "PRIVMSG " <> rest ->
            [channel, ":" <> message] = rest |> String.split(" ", global: false)

            channel = Connection.call({ :get, :channel, server, channel })
            event   = Event.Message.new(
              server:  server,
              channel: channel,
              user:    User.parse(from),
              content: message)

            handle(plugins, channel, event)

          "NICK " <> rest ->
            handle(plugins, server, Event.Nick[server: server, old: from.nick, new: rest])

          << a :: utf8, b :: utf8, c :: utf8, ?\s :: utf8, rest :: binary >> when a in ?0 .. ?9 and
                                                                                  b in ?0 .. ?9 and
                                                                                  c in ?0 .. ?9 ->
            handle(plugins, server, Event.Numeric.make(server,
              binary_to_integer(<< a :: utf8, b :: utf8, c :: utf8 >>), rest))

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
      { _, Plugin[pid: ^pid] } -> true
      { _, _ }                 -> false
    end

    case plugin do
      { name, plugin } ->
        case plugin.module.start_link(options) do
          { :ok, pid } ->
            { :noreply, state.plugins(Dict.put(plugins, name, plugin.pid(pid))) }

          _ ->
            { :noreply, state }
        end

      nil ->
        { :noreply, state }
    end
  end

  defp handle(plugins, thing, event) do
    Enum.each plugins, fn { name, Plugin[module: module] } ->
      if Data.contains?(thing.plugins, name) do
        :gen_server.cast(module, { :handle, event })
      end
    end
  end
end

defmodule Mana.Plugin do
  use Behaviour

  defcallback init(Keyword.t) :: { :ok, term } | { :error, term }
  defcallback handle(record, term) :: { :ok, term } | { :error, term }
  defcallback call(term, term) :: { :ok, term, term } | { :error, term }

  use GenServer.Behaviour

  defrecord State, options: nil, plugins: []
  defrecord Plugin, name: nil, module: nil, state: nil

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
    :gen_server.call(__MODULE__, { :call, plugin, what })
  end

  def handle_call({ :register, name, module }, _from, State[options: options, plugins: plugins] = state) do
    case module.init(options) do
      { :ok, plugin_state } ->
        plugin = Plugin[name: name, module: module, state: plugin_state]

        { :reply, :ok, state.plugins(Dict.put(plugins, name, plugin)) }

      { :error, _ } = e ->
        { :reply, e, state }
    end
  end

  def handle_call({ :call, name, what }, _from, State[plugins: plugins] = state) do
    plugin = Dict.get(plugins, name)

    case plugin.module.call(what, plugin.state) do
      { :ok, result, plugin_state } ->
        { :reply, result, state.plugins(Dict.put(plugins, plugin.name, plugin.state(plugin_state))) }

      { :error, reason, plugin_state } ->
        { :reply, { :error, reason }, state.plugins(Dict.put(plugins, plugin.name, plugin.state(plugin_state))) }
    end
  end

  def handle_call({ :event, name, event }, _from, State[plugins: plugins] = state) do
    plugin = Dict.get(plugins, name)
    state  = case plugin.module.event(event, plugin.state) do
      { :ok, plugin_state } ->
        state.plugins(Dict.put(plugins, plugin.name, plugin.state(plugin_state)))

      { :error, plugin_state } ->
        state.plugins(Dict.put(plugins, plugin.name, plugin.state(plugin_state)))
    end

    { :noreply, state }
  end

  def handle_info({ :receive, server, line }, State[plugins: plugins] = state) do
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

            plugins = handle(plugins, channel, event)
            state   = state.plugins(plugins)

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

            plugins = handle(plugins, channel, event)
            state   = state.plugins(plugins)


          "PRIVMSG " <> rest ->
            [channel, ":" <> message] = rest |> String.split(" ", global: false)

            channel = Mana.Connection.call({ :get, :channel, server, channel })
            event   = Mana.Event.Message.new(
              server:  server,
              channel: channel,
              user:    Mana.User.parse(from),
              content: message)

            plugins = handle(plugins, channel, event)
            state   = state.plugins(plugins)

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

  def handle_info({ :EXIT, _, reason }, _state) do
    IO.inspect reason

    { :noreply, _state }
  end

  defp handle(plugins, channel, event) do
    Enum.reduce plugins, plugins, fn { name, Plugin[] = plugin }, acc ->
      if Data.contains?(channel.plugins, name) do
        plugin_state = case plugin.module.handle(event, plugin.state) do
          { _, plugin_state } ->
            plugin_state
        end

        Dict.put(acc, name, plugin.state(plugin_state))
      else
        acc
      end
    end
  end

  defp connected(server) do
    Enum.each server.channels, fn { name, _ } ->
      server.send "JOIN #{name}"
    end
  end
end

defmodule Mana do
  use Application.Behaviour
  use Supervisor.Behaviour

  def start(_type, options) do
    :supervisor.start_link(__MODULE__, options)
  end

  def stop(_) do
    Process.exit __MODULE__
  end

  def init(options) do
    supervise [ worker(Mana.Connection, [options]),
                worker(Mana.Plugin, [options]) ],
      strategy: :one_for_one
  end

  def load(path) do
    Enum.each Path.wildcard("#{path}/**/*.exs"), fn file ->
      case File.read(file) do
        { :ok, content } ->
          IO.write "Loading #{file}..."

          try do
            Code.compile_string "import Mana.DSL; #{content}", file

            IO.puts " done"
          catch kind, reason ->
            IO.puts " error"

            :erlang.raise(kind, reason, System.stacktrace)
          end
      end
    end
  end

  def plugin(name, module) do
    case Mana.Plugin.call({ :register, name, module }) do
      :ok ->
        :ok

      :restarting ->
        :restarting

      { :error, reason } ->
        raise reason
    end
  end

  def server(server) do
    Mana.Connection.call({ :connect, server })
  end

  def join(channel) do
    Mana.Connection.call({ :join, channel })
  end
end

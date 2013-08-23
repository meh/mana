defmodule Mana.DSL do
  defmacro defserver(host, do: block) do
    quote do
      defserver(unquote(host), 6667, [], do: unquote(block))
    end
  end

  defmacro defserver(host, port, do: block) when is_integer(port) do
    quote do
      defserver(unquote(host), unquote(port), [], do: unquote(block))
    end
  end

  defmacro defserver(host, options, do: block) when is_list(options) do
    quote do
      defserver(unquote(host), 6667, unquote(options), do: unquote(block))
    end
  end

  defmacro defserver(host, port, options, do: block) when is_integer(port) do
    quote do
      server = Mana.Server.make(unquote(host), unquote(port), unquote(options))
      Mana.server server

      unquote(block)

      server = nil
    end
  end

  defmacro defchannel(name, options // []) do
    quote do
      Mana.join Mana.Channel.make(server, unquote(name), unquote(options))
    end
  end

  defmacro defplugin(name, do: block) do
    module = name |> to_string |> String.capitalize |> binary_to_atom

    quote do
      module = Module.concat(Mana.Plugin, unquote(module))

      defmodule module do
        use Mana.Plugin

        unquote(block)
      end

      Mana.plugin(unquote(name), module)
    end
  end
end

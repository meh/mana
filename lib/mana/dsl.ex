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
      unquote(block)
      Mana.connect server
    end
  end

  defmacro nick(value) do
    quote do
      server = server.nick(unquote(value))
    end
  end

  defmacro user(value) do
    quote do
      server = server.user(unquote(value))
    end
  end

  defmacro realname(value) do
    quote do
      server = server.realname(unquote(value))
    end
  end

  defmacro defchannel(name, options // []) do
    quote do
      server = server.channels(Dict.put(server.channels, unquote(name),
        Mana.Channel.make(server, unquote(name), unquote(options))))
    end
  end

  defmacro defplugin(name, do: block) do
    module = name |> to_string |> String.capitalize |> binary_to_atom

    quote do
      module = Module.concat(Mana.Plugin, unquote(module))

      :code.delete(module)

      Enum.each :code.all_loaded, fn { mod, _ } ->
        if to_string(mod) |> String.starts_with?(to_string(module) <> ".") do
          :code.delete(mod)
        end
      end

      defmodule module do
        use Mana.Plugin

        unquote(block)
      end

      Mana.plugin(unquote(name), module)
    end
  end
end

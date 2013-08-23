defrecord Mana.Channel, server: nil, name: nil, plugins: [] do
  alias Data.Set
  alias Mana.Server

  def make(server, name, options // []) do
    Mana.Channel[ server:  server.name,
                  name:    name,
                  plugins: Set.union(server.plugins, options[:plugins] || []) ]
  end

  def message(what, Mana.Channel[server: server, name: name]) do
    server.send "PRIVMSG #{name} :#{what}"
  end
end

defimpl String.Chars, for: Mana.Channel do
  def to_string(Mana.Channel[name: name]) do
    name
  end
end

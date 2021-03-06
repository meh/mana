defrecord Mana.Event.Numeric, server: nil, number: nil, details: nil do
  def make(server, number, rest) do
    __MODULE__[server: server, number: number, details: rest]
  end
end

defrecord Mana.Event.Nick, server: nil, user: nil, nick: nil

defrecord Mana.Event.Message, server: nil, channel: nil, user: nil, content: nil do
  def reply(message, __MODULE__[server: server, channel: channel]) do
    Mana.Connection.send "PRIVMSG #{channel} :#{message}", to: server
  end
end

defrecord Mana.Event.Leave, server: nil, channel: nil, user: nil, reason: nil
defrecord Mana.Event.Join, server: nil, channel: nil, user: nil
defrecord Mana.Event.Kick, server: nil, channel: nil, user: nil, kicked: nil, reason: nil
defrecord Mana.Event.Ban, server: nil, channel: nil, user: nil, mask: nil
defrecord Mana.Event.Quit, server: nil, user: nil, reason: nil

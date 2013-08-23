defrecord Mana.Message, server: nil, channel: nil, from: nil, content: nil do
  def reply(message, Mana.Message[server: server, channel: channel]) do
    Mana.send "PRIVMSG #{channel} #{message}", to: server
  end
end

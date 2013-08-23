defplugin :logger do
  defrecord User, nick: nil, user: nil, host: nil
  defrecord Message, in: nil, user: nil, date: nil, content: nil

  def init(_options) do
    { :ok, Dexts.Table.new("logs.dat", type: :duplicate_bag, index: 1) }
  end

  def handle(Event.Message[server: server, channel: channel, user: user, content: content], table) do
    table.write Message[
      in:      { server.name, channel.name },
      user:    User[nick: user.nick, user: user.user, host: user.host],
      date:    DateTime.now,
      content: content ]

    { :ok, table }
  end

  def handle(_, table) do
    { :ok, table }
  end

  def call({ :get, server, channel }, table) do
    { :ok, table.read({ server, channel }), table }
  end
end

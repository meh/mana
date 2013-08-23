defplugin :logger do
  defrecord User, nick: nil, user: nil, host: nil do
    @type t :: User[ nick: String.t,
                     user: String.t,
                     host: String.t ]
  end

  defrecord Message, in: nil, date: nil, user: nil, content: nil do
    @type t :: Message[ in:      { server :: String.t, channel :: String.t },
                        date:    DateTime.t,
                        user:    User.t,
                        content: String.t ]
  end

  defrecord Join, in: nil, date: nil, user: nil do
    @type t :: Join[ in:   { server :: String.t, channel :: String.t },
                     date: DateTime.t,
                     user: User.t ]
  end

  defrecord Leave, in: nil, date: nil, user: nil, reason: nil do
    @type t :: Leave[ in:     { server :: String.t, channel :: String.t },
                      date:   DateTime.t,
                      user:   User.t,
                      reason: String.t | nil ]
  end

  def init(_options) do
    { :ok, Dexts.Table.new("logs.dat", type: :duplicate_bag, index: 1) }
  end

  def handle(Event.Message[server: server, channel: channel, user: user, content: content], table) do
    table.write Message[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    User[nick: user.nick, user: user.user, host: user.host],
      content: content ]

    { :ok, table }
  end

  def handle(Event.Join[server: server, channel: channel, user: user], table) do
    table.write Join[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    User[nick: user.nick, user: user.user, host: user.host] ]

    { :ok, table }
  end

  def handle(Event.Leave[server: server, channel: channel, user: user, reason: reason], table) do
    table.write Leave[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    User[nick: user.nick, user: user.user, host: user.host],
      reason:  reason ]

    { :ok, table }
  end

  def handle(_, _table) do
    { :ok, _table }
  end

  def call({ :get, server, channel }, table) do
    { :ok, table.read({ server, channel }), table }
  end

  def call({ :query, query }, table) do
    { :ok, table.select(query).values, table }
  end
end

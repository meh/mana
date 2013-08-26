defplugin :logger do
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

  def init(options) do
    directory = options[:data] || "."
    File.mkdir directory

    path  = Path.join(directory, "logs.dat")
    table = Dexts.Table.new(path,
      type: :duplicate_bag,
      index: 1,
      automatic: false,
      asynchronous: true,
      save_every: 30_000)

    { :ok, table }
  end

  def terminate(_reason, table) do
    table.save
    table.close
  end

  def handle(Event.Message[server: server, channel: channel, user: user, content: content], table) do
    table.write Message[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    user,
      content: content ]

    { :ok, table }
  end

  def handle(Event.Join[server: server, channel: channel, user: user], table) do
    table.write Join[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    user ]

    { :ok, table }
  end

  def handle(Event.Leave[server: server, channel: channel, user: user, reason: reason], table) do
    table.write Leave[
      in:      { server.name, channel.name },
      date:    DateTime.now,
      user:    user,
      reason:  reason ]

    { :ok, table }
  end

  def call({ :get, server, channel }, table) when is_binary(server) and is_binary(channel) do
    { :ok, table.read({ server, channel }), table }
  end

  def call({ :query, query }, table) do
    if selector = table.select(query) do
      { :ok, selector.values, table }
    else
      { :ok, [], table }
    end
  end

  def call(:list, table) do
    { :ok, table.keys, table }
  end
end

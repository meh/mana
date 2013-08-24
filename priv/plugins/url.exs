defplugin :url do
  @regex %R"""x
  # RFC-3986 URI component:  URI
  [A-Za-z][A-Za-z0-9+\-.]* :                                      # scheme ":"
  (?: //                                                          # hier-part
    (?: (?:[A-Za-z0-9\-._~!$&'()*+,;=:]|%[0-9A-Fa-f]{2})* @)?
    (?:
      \[
      (?:
        (?:
          (?:                                                    (?:[0-9A-Fa-f]{1,4}:)    {6}
          |                                                   :: (?:[0-9A-Fa-f]{1,4}:)    {5}
          | (?:                            [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:)    {4}
          | (?: (?:[0-9A-Fa-f]{1,4}:){0,1} [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:)    {3}
          | (?: (?:[0-9A-Fa-f]{1,4}:){0,2} [0-9A-Fa-f]{1,4})? :: (?:[0-9A-Fa-f]{1,4}:)    {2}
          | (?: (?:[0-9A-Fa-f]{1,4}:){0,3} [0-9A-Fa-f]{1,4})? ::    [0-9A-Fa-f]{1,4}:
          | (?: (?:[0-9A-Fa-f]{1,4}:){0,4} [0-9A-Fa-f]{1,4})? ::
          ) (?:
              [0-9A-Fa-f]{1,4} : [0-9A-Fa-f]{1,4}
            | (?: (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?) \.){3}
                  (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
            )
        |   (?: (?:[0-9A-Fa-f]{1,4}:){0,5} [0-9A-Fa-f]{1,4})? ::    [0-9A-Fa-f]{1,4}
        |   (?: (?:[0-9A-Fa-f]{1,4}:){0,6} [0-9A-Fa-f]{1,4})? ::
        )
      | [Vv][0-9A-Fa-f]+\.[A-Za-z0-9\-._~!$&'()*+,;=:]+
      )
      \]
    | (?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
         (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)
    | (?:[A-Za-z0-9\-._~!$&'()*+,;=]|%[0-9A-Fa-f]{2})*
    )
    (?: : [0-9]* )?
    (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
  | /
    (?:    (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+
      (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
    )?
  |        (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})+
      (?:/ (?:[A-Za-z0-9\-._~!$&'()*+,;=:@]|%[0-9A-Fa-f]{2})* )*
  |
  )
  (?:\? (?:[A-Za-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9A-Fa-f]{2})* )?   # [ "?" query ]
  (?:\# (?:[A-Za-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9A-Fa-f]{2})* )?   # [ "#" fragment ]
  """

  defrecord Log, in: nil, index: nil, date: nil, user: nil, info: nil

  alias Data.Dict
  alias Data.Seq

  def init(options) do
    directory = options[:data] || "."
    File.mkdir directory

    path  = Path.join(directory, "urls.dat")
    table = Dexts.Table.new(path,
      type: :duplicate_bag,
      index: 1,
      automatic: false,
      asynchronous: true,
      save_every: 30_000)

    indices = Seq.reduce table, HashDict.new, fn logs, acc ->
      Seq.reduce logs, acc, fn Log[in: location, index: index], acc ->
        Dict.update acc, location, 0, &(if index > &1, do: index, else: &1)
      end
    end

    { :ok, { indices, table } }
  end

  def terminate(_reason, { _, table }) do
    table.save
    table.close
  end

  def handle(Event.Message[content: ":url " <> rest, server: server, channel: channel] = msg, { indices, table } = _state) do
    last = Dict.get(indices, { server.name, channel.name }, 0)

    case rest do
      "shorten" ->
        by_index(table, server.name, channel.name, last).info |> shorten |> msg.reply

      "shorten " <> n ->
        by_index(table, server.name, channel.name, binary_to_integer(n)).info |> shorten |> msg.reply
    end

    { :ok, _state }
  end

  def handle(Event.Message[server: server, channel: channel, user: user, content: content], { indices, table }) do
    last = Dict.get(indices, { server.name, channel.name }, 0)
    urls = Regex.scan(@regex, content)

    Enum.each Enum.with_index(urls), fn { [uri], index } ->
      table.write(Log[ in:    { server.name, channel.name },
                       index: last + index + 1,
                       date:  DateTime.now,
                       user:  user,
                       info:  URI.parse(uri) ])
    end

    { :ok, { indices |> Dict.put({ server.name, channel.name }, last + length(urls)), table } }
  end

  def call({ :get, server, channel }, { _, table } = _state) when is_binary(server) and is_binary(channel) do
    { :ok, table.read({ server, channel }), _state }
  end

  def call({ :get, server, channel, index }, { _, table } = _state) when is_binary(server) and is_binary(channel) do
    case by_index(table, server, channel, index) do
      nil ->
        { :error, :missing, _state }

      url ->
        { :ok, url, _state }
    end
  end

  def call({ :query, query }, { _, table } = _state) do
    case table.select(query) do
      nil ->
        { :ok, [], _state }

      selector ->
        { :ok, selector.values, _state }
    end
  end

  defp by_index(table, server, channel, index) do
    case table.match(Log[in: { server, channel }, index: index, _: :_], whole: true) do
      nil ->
        nil

      match ->
        match.values |> Enum.first
    end
  end

  defp shorten(uri) do
    uri = to_string(uri)

    case :httpc.request("http://is.gd/create.php?format=simple&url=#{URI.encode(uri)}" |> String.to_char_list!) do
      { :ok, { _, _, body } } ->
        body |> String.from_char_list!

      { :error, _ } ->
        uri
    end
  end
end

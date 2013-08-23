defrecord Mana.Server, name: nil, host: nil, port: nil, secure: false, password: nil, channels: [], plugins: [] do
  alias Data.Set
  alias Data.Set.List, as: ListSet

  def make(host, port // 6667, options // []) do
    Mana.Server[ name:     options[:name] || host,
                 host:     host,
                 port:     port,
                 secure:   options[:secure] || false,
                 password: options[:password],
                 plugins:  ListSet.new(options[:plugins] || []) ]
  end

  def secure?(Mana.Server[secure: secure]) do
    secure
  end

  def send(what, self) do
    Mana.Connection.send what, to: self
  end
end

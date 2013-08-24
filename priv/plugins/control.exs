defplugin :control do
  defrecord State, identified: []

  def init(_options) do
    { :ok, State[] }
  end
end

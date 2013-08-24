defplugin :control do
  defrecord State, identified: []

  def init(_options) do
    { :ok, State[] }
  end

  def handle(Event.Message[content: "I like trains"] = msg, _state) do
    msg.reply "me too! :DDDDDDDD"

    { :ok, _state }
  end
end

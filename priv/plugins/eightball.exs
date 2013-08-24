defplugin :control do
  defrecord State, identified: []

  def init(_options) do
    { :ok, State[] }
  end

  def handle(Event.Message[content: "Manaball, " <> _] = msg, _state) do
    reply = (
      case :random.uniform(5) do
        1 -> "If so, expect your powers to weaken gradually for the foreseeable future." 
        2 -> "Mana low. Ask again after succubi forces have retreated."
        3 -> "Unquestionably. Don't question it."
        4 -> "U-unsure. S-sorry I'm such a baka manabot ;_;"
        5 -> "The furious wrath of the mystic world conspires to ensure it is not so."
      end)

    msg.reply reply

    { :ok, _state }
  end
end

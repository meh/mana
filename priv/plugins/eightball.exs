defplugin :eightball do
  def handle(msg, _state) do
    if String.starts_with?(msg.content, [msg.server.nick <> ":", msg.server.nick <> ","])
       and String.ends_with?(msg.content, "?") do
      case :random.uniform(5) do
        1 -> "If so, expect your powers to weaken gradually for the foreseeable future." 
        2 -> "Mana low. Ask again after succubi forces have retreated."
        3 -> "Unquestionably. Don't question it."
        4 -> "U-unsure. S-sorry I'm such a baka manabot ;_;"
        5 -> "The furious wrath of the mystic world conspires to ensure it is not so."
      end |> msg.reply
    end

    { :ok, _state }
  end
end

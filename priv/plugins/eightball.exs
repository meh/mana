defplugin :eightball do
  def handle(Event.Message[content: content, server: Server[nick: nick]] = msg, _state) do
    if String.starts_with?(content, [nick <> ":", nick <> ","]) and
       String.ends_with?(content, "?") do
      case :random.uniform(27) do
        1 -> "If so, expect your powers to weaken gradually for the foreseeable future."
        2 -> "Mana low. Ask again after succubi forces have retreated."
        3 -> "Unquestionably. Don't question it."
        4 -> "U-unsure. S-sorry I'm such a baka manabot ;_;"
        5 -> "The furious wrath of the mystic world conspires to ensure it is not so."
        6 -> "Fuck you. Fuck you for asking me that. What the hell is your problem?"
        7 -> "Leave the channel for one month. Go to the woods and contemplate your own "
             <> "question. If you can not answer, then I will not."
        8 -> "Umm... yeah! I think so :3"
        9 -> "Oh, totally. No question."
        10 -> "A wizard is as lost without love as a sloth is lost without a bicycle."
        11 -> "I can not answer your question without violating rule 5."
        12 -> "I have consulted with the wizards council and our decision is no."
        13 -> "I suspect so, but the runes seem to have no opinion on the matter."
        14 -> "I'm actually a little busy right now. It's the anniversary of the day I met "
              <> "my waifu :D"
        15 -> "Let me answer your question with another question. Does every girl get hit "
              <> "on all day every day by alphas?"
        16 -> "lol jus b urself silly :)"
        17 -> "Why do you always ask me this stupid shit? Why don't you go bother Teslabot "
              <> "for a change?"
        18 -> "It is known."
        19 -> "\x{201C}Fo shizzle\x{201D} - Snoop Lion"
        20 -> "Could be, could be. You never know with wizards."
        21 -> "What kind of a normie question is that?"
        22 -> "Turn in your wizard scepter. Give me back your wand. You're out of the club."
        23 -> "What would Gandalf say? I think he'd say yes. At least, I like to think he'd "
              <> "say yes."
        24 -> "Umm... yeah? I dunno. Anyway, you guys wanna get drunk?"
        25 -> "A true wizard is not concerned with such trivialities."
        26 -> "I don't think it would be in either of our interests for that to be the case."
        27 -> "I take off my robe and wizard hat... Oh, shit! Sorry, wrong window!"
      end |> msg.reply
    end

    { :ok, _state }
  end
end

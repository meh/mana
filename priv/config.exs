defserver "irc.wizardchan.org", plugins: [:control, :logger, :url, :eightball, :regex, :autojoin] do
  nick     "Mana"
  user     "Mana"
  realname "Succubus begone."

  defchannel "#wiz",  plugins: [:succubus]
  defchannel "#anon", plugins: [:anonchat]
  defchannel "#ask",  plugins: [:ask]

  defchannel "#code", plugins: [:succubus]
end

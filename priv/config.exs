defserver "irc.wizardchan.org", plugins: [:control, :logger, :url, :regex, :autojoin] do
  nick     "Mana"
  user     "Mana"
  realname "Succubus begone."

  defchannel "#wiz",  plugins: [:succubus, :eightball]
  defchannel "#anon", plugins: [:anonchat]
  defchannel "#ask",  plugins: [:ask]

  defchannel "#code", plugins: [:succubus, :eightball]
end

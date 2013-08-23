defserver "irc.wizardchan.org", plugins: [:control, :logger] do
#  defchannel "#wiz",  plugins: [:succubus, :regex]
  #defchannel "#anon", plugins: [:anonchat]
  #defchannel "#ask",  plugins: [:ask]

  defchannel "#code", plugins: [:succubus, :regex]
end

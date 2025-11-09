define variable inputParam as character.
os-command value("ls -l " + inputParam).

run value (inputParam + "/proc.p").


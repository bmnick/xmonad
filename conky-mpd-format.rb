#!/usr/bin/env ruby 
status = %x{mpc}
lines = status.lines.to_a
stat_image = if lines[1]
    play_status = lines[1].split[0]
    case play_status
    when "[playing]"
        "^i(/home/bnicholas/.xmonad/dzen2/005b_57.xbm)"
    when "[paused]"
        "^i(/home/bnicholas/.xmonad/dzen2/005b_59.xbm)"
    end
else
    "^i(/home/bnicholas/.xmonad/dzen2/005b_58.xbm)"
end

out = "^ca(1,mpc toggle)^ca(4, mpc next)^ca(5, mpc prev)^fg(\#d7ff5f)"
out << stat_image
out << " ^fg(\#5fffff)"
if lines.count > 1
    out << lines[0].chomp
else
    out << "---"
end
out << "^ca()^ca()^ca()"

print out


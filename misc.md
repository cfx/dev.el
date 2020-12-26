##### Disable X11 login (Debian 10):
```
sudo systemctl disable lightdm.service
```

##### Natural scrolling for mouse:

Add `Option "NaturalScrolling" "on"` to `/usr/share/X11/xorg.conf.d/40-libinput.conf`.

```
Section "InputClass"
        Identifier "libinput pointer catchall"
        MatchIsPointer "on"
        MatchDevicePath "/dev/input/event*"
        Driver "libinput"
        Option "NaturalScrolling" "on"
EndSection
```

##### Map Caps Lock to Control (X11)
Add `XKBOPTIONS="ctrl:nocaps"` to `/etc/default/keyboard`




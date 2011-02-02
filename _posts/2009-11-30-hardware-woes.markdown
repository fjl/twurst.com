---
layout: post
title: Hardware Woes
---
Recently, I have acquired a Samsung N140 netbook.
As one of these guys who buy a laptop without checking whether their favourite operating system actually supports all the fancy hardware that is built into it, I was surprised that almost everything worked out of the box. Up till now, there were three annoying issues restraining me from using the device productively. Two of them have been fixed in Linux-2.6.32 and I am very happy about that.

The ath9k driver was broken in the 2.6.31 release resulting in frequent disconnects. With the new kernel, only two very short connectivity outages have happened in three days of constant use.

Secondly, backlight could only be set by utilising the `setpci` command with some hardware address and, of course, superuser rights. I don't change display brightness very often, but it is rather inconvenient having to type

    $ sudo setpci -s 00:02.1 F4.B=FF

in order to turn it up. The upgrade made `xbacklight` work again, so now I can say

    $ xbacklight -set 100

which is way better.

The third major problem is the worst, and it is also the one that still persists.
Shortly after turning on the device or resuming from suspend-to-ram everything freezes for about 10 seconds. Two kernel bugs have been filed about this ([bug1][bug1], [bug2][bug2]), but there is no real solution at this point. I have upgraded the BIOS, which required installing Windows XP to the swap partition (damn you, Samsung). The discussion following the reports is a bit over my head, but apparently some ACPI code in the BIOS talks to the harddrive directly, causing an ATA exception which forces a reset. The relevant output of `dmesg` is:

    [ 3783.000139] ata1.00: exception Emask 0x0 SAct 0x0 SErr 0x0 action 0x6 frozen
    [ 3783.000159] ata1.00: failed command: WRITE DMA
    [ 3783.000186] ata1.00: cmd ca/00:08:fe:a3:5c/00:00:00:00:00/e2 tag 0 dma 4096 out
    [ 3783.000193]          res 40/00:00:00:00:00/00:00:00:00:00/00 Emask 0x4 (timeout)
    [ 3783.000206] ata1.00: status: { DRDY }
    [ 3788.049070] ata1: link is slow to respond, please be patient (ready=0)
    [ 3793.047100] ata1: device not ready (errno=-16), forcing hardreset
    [ 3793.047122] ata1: soft resetting link

Next week, I will try to apply some of the patches from bugzilla and see if it works better.
This seems to be an issue specific to Samsung drives.

There is a [page on the Arch Linux wiki][arch] about the N140 where workarounds for common problems are listed. This might be helpful for anybody in possession of a machine with similar issues.

All in all, if you plan to buy a netbook and want to run Linux, do not choose the Samsung N140.

[bug1]: http://bugzilla.kernel.org/show_bug.cgi?id=14314
[bug2]: http://bugzilla.kernel.org/show_bug.cgi?id=13416
[arch]: http://wiki.archlinux.org/index.php/Samsung_N140

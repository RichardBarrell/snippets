#359e800000-359e98a000 r-xp 00000000 fd:00 15176                          /lib64/libc-2.12.so
#Size:               1576 kB
#Rss:                 384 kB
#Pss:                  18 kB
#Shared_Clean:        384 kB
#Shared_Dirty:          0 kB
#Private_Clean:         0 kB
#Private_Dirty:         0 kB
#Referenced:          384 kB
#Anonymous:             0 kB
#AnonHugePages:         0 kB
#Swap:                  0 kB
#KernelPageSize:        4 kB
#MMUPageSize:           4 kB

#359e621000-359e622000 rw-p 00000000 00:00 0


def parse_smaps(lines):
    item = None
    for line in lines:
        fields = line.strip().split()
        if not fields:
            continue
        if len(fields) >= 5:
            if not (item is None):
                yield item
            item = {}
            item['addr'] = fields[0]
            item['mode'] = fields[1]
            if len(fields) >= 6:
                item['file'] = " ".join(fields[6:])
            else:
                item['file'] = None
        else:
            key, number, suffix = fields
            assert suffix == "kB"
            item[key[:-1]] = int(number)
    if item:
        yield item

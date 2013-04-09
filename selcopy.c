#include <sys/types.h>
#include <sys/xattr.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv) {
        size_t attrvaluesize = 1 << 10;
        char *attrvalue = malloc(attrvaluesize);
        size_t pathsize = 1 << 10;
        char *path = malloc(pathsize);
        char *path_plus;
        char *suffix;
        ssize_t attrlen;
        size_t prefixlen, suffixlen;
        int argv_pos;
        int setxattr_rv;
        if (attrvalue == NULL) { perror("damn"); return 1; }
        if (path == NULL) { perror("damn"); return 1; }
        if (argc < 2) {
                fprintf(stderr, "Usage: selcopy /prefix file1 file2 file3...\n");
                return 1;
        }
        prefixlen = strlen(argv[1]);
        memcpy(path, argv[1], prefixlen);
        path_plus = path + prefixlen;
        for (argv_pos=2; argv_pos < argc; argv_pos++) {
                suffix = argv[argv_pos];
                suffixlen = strlen(suffix);
                if ((prefixlen + suffixlen + 1) > pathsize) {
                        fprintf(stderr, "name %s%s longer than %zd\n", argv[1], suffix, pathsize);
                        continue;
                }
                memcpy(path_plus, suffix, suffixlen);
                path_plus[suffixlen] = '\0';
                attrlen = lgetxattr(suffix, "security.selinux", attrvalue, attrvaluesize);
                if (attrlen < 0) {
                        fprintf(stderr, "lgetxattr ");
                        perror(suffix);
                        continue;
                }
                setxattr_rv = lsetxattr(path, "security.selinux", attrvalue, attrlen, 0);
                if (setxattr_rv != 0) {
                        fprintf(stderr, "lsetxattr ");
                        perror(suffix);
                        continue;
                }
        }
        free(path);
        free(attrvalue);
        return 0;
}

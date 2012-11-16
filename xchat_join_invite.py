__module_name__ = "join_invite"
__module_version__ = "0.1"
__module_description__ = "Join channels immediately when invited."

import xchat
# import pickle

# servers = set()

# def add_server(word, word_eol, userdata=None):
#     for server in word[1:]:
#         servers.add(server)
#     print "Suggestible servers are:", " ".join(map(repr, servers))
#     return xchat.EAT_ALL

# xchat.hook_command("SUGGEST_ADD", add_server,
#                    help="/SUGGEST_ADD <server> become suggestible on server.")

# def rem_server(word, word_eol, userdata=None):
#     for server in word[1:]:
#         if server in servers:
#             servers.remove(server)
#         else:
#             print "%r is not in servers." % server
#     print "Suggestible servers are:", " ".join(map(repr, servers))
#     return xchat.EAT_ALL

# xchat.hook_command("SUGGEST_REM", rem_server,
#                    help="/SUGGEST_REM <server> be not suggestible on server.")

def suggestible(word, word_eol, userdata=None):
    context = xchat.get_context()
    connected_to = context.get_info("server").lower()
    context.command("JOIN " + word[3])
    return xchat.EAT_XCHAT

xchat.hook_server("INVITE", suggestible)

print "Becoming suggestible"

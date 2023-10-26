
#include <stdlib.h>
#include <stdio.h>
#include <gensio/gensio.h>

char *
gensio_loginfo_to_str(void *vloginfo) {
    struct gensio_loginfo *l = vloginfo;
    size_t len, pos;
    char dummy[10], *s;
    va_list argcopy;
    va_copy(argcopy, l->args);

    len = snprintf(dummy, 10, "%s: ",
		   gensio_log_level_to_str(l->level));
    len += vsnprintf(dummy, 10, l->str, l->args);
    if (len == 0)
	return NULL;
    s = malloc(len + 1);
    if (!s)
	return NULL;
    pos = snprintf(s, len + 1, "%s: ",
		   gensio_log_level_to_str(l->level));
    vsnprintf(s + pos, len - pos + 1, l->str, argcopy);
    va_end(argcopy);
    return s;
}

char *
gensio_parmlog_to_str(void *vloginfo) {
    struct gensio_parmlog_data *l = vloginfo;
    size_t len;
    char dummy[10], *s;
    va_list argcopy;
    va_copy(argcopy, l->args);

    len = vsnprintf(dummy, 10, l->log, l->args);
    if (len == 0)
	return NULL;
    s = malloc(len + 1);
    if (!s)
	return NULL;
    vsnprintf(s, len + 1, l->log, argcopy);
    va_end(argcopy);
    return s;
}

void
gensio_free_loginfo_str(char *str)
{
    free(str);
}

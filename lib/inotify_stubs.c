#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#ifdef __linux__

#include <sys/inotify.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

CAMLprim value bilk_inotify_init(value unit) {
    CAMLparam1(unit);
    int fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (fd < 0)
        uerror("inotify_init1", Nothing);
    CAMLreturn(Val_int(fd));
}

CAMLprim value bilk_inotify_add_watch(value fd_v, value path_v, value mask_v) {
    CAMLparam3(fd_v, path_v, mask_v);
    int fd = Int_val(fd_v);
    const char *path = String_val(path_v);
    uint32_t mask = (uint32_t)Int64_val(mask_v);
    int wd = inotify_add_watch(fd, path, mask);
    if (wd < 0) {
        if (errno == ENOSPC)
            CAMLreturn(Val_int(-1));  /* signal watch limit reached */
        uerror("inotify_add_watch", caml_copy_string(path));
    }
    CAMLreturn(Val_int(wd));
}

CAMLprim value bilk_inotify_rm_watch(value fd_v, value wd_v) {
    CAMLparam2(fd_v, wd_v);
    int fd = Int_val(fd_v);
    int wd = Int_val(wd_v);
    inotify_rm_watch(fd, wd);
    CAMLreturn(Val_unit);
}

/* Read inotify events into a list of (wd, mask, name) tuples.
   Returns an empty list if no events are available (EAGAIN). */
CAMLprim value bilk_inotify_read(value fd_v) {
    CAMLparam1(fd_v);
    CAMLlocal4(result, cons, tuple, name_val);
    int fd = Int_val(fd_v);

    char buf[4096]
        __attribute__((aligned(__alignof__(struct inotify_event))));

    ssize_t len = read(fd, buf, sizeof(buf));
    if (len < 0) {
        if (errno == EAGAIN)
            CAMLreturn(Val_emptylist);
        uerror("read(inotify)", Nothing);
    }

    result = Val_emptylist;
    /* Walk events backwards so we can prepend and get correct order */
    /* First, count events and store offsets */
    int offsets[256];
    int count = 0;
    const char *ptr = buf;
    while (ptr < buf + len && count < 256) {
        offsets[count++] = (int)(ptr - buf);
        const struct inotify_event *ev = (const struct inotify_event *)ptr;
        ptr += sizeof(struct inotify_event) + ev->len;
    }

    /* Build list in reverse */
    for (int i = count - 1; i >= 0; i--) {
        const struct inotify_event *ev =
            (const struct inotify_event *)(buf + offsets[i]);
        tuple = caml_alloc_tuple(3);
        Store_field(tuple, 0, Val_int(ev->wd));
        Store_field(tuple, 1, caml_copy_int64((int64_t)ev->mask));
        if (ev->len > 0)
            name_val = caml_copy_string(ev->name);
        else
            name_val = caml_copy_string("");
        Store_field(tuple, 2, name_val);

        cons = caml_alloc(2, 0);
        Store_field(cons, 0, tuple);
        Store_field(cons, 1, result);
        result = cons;
    }
    CAMLreturn(result);
}

/* Mask constants */
CAMLprim value bilk_in_modify(value unit)   { (void)unit; return caml_copy_int64(IN_MODIFY); }
CAMLprim value bilk_in_create(value unit)   { (void)unit; return caml_copy_int64(IN_CREATE); }
CAMLprim value bilk_in_delete(value unit)   { (void)unit; return caml_copy_int64(IN_DELETE); }
CAMLprim value bilk_in_moved_to(value unit) { (void)unit; return caml_copy_int64(IN_MOVED_TO); }
CAMLprim value bilk_in_moved_from(value unit) { (void)unit; return caml_copy_int64(IN_MOVED_FROM); }
CAMLprim value bilk_in_isdir(value unit)    { (void)unit; return caml_copy_int64(IN_ISDIR); }

#else  /* not __linux__ */

#include <caml/fail.h>

CAMLprim value bilk_inotify_init(value unit) {
    (void)unit;
    return Val_int(-1);
}

CAMLprim value bilk_inotify_add_watch(value fd_v, value path_v, value mask_v) {
    (void)fd_v; (void)path_v; (void)mask_v;
    return Val_int(-1);
}

CAMLprim value bilk_inotify_rm_watch(value fd_v, value wd_v) {
    (void)fd_v; (void)wd_v;
    return Val_unit;
}

CAMLprim value bilk_inotify_read(value fd_v) {
    (void)fd_v;
    return Val_emptylist;
}

CAMLprim value bilk_in_modify(value unit)     { (void)unit; return caml_copy_int64(0); }
CAMLprim value bilk_in_create(value unit)     { (void)unit; return caml_copy_int64(0); }
CAMLprim value bilk_in_delete(value unit)     { (void)unit; return caml_copy_int64(0); }
CAMLprim value bilk_in_moved_to(value unit)   { (void)unit; return caml_copy_int64(0); }
CAMLprim value bilk_in_moved_from(value unit) { (void)unit; return caml_copy_int64(0); }
CAMLprim value bilk_in_isdir(value unit)      { (void)unit; return caml_copy_int64(0); }

#endif

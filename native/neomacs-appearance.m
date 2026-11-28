// SPDX-License-Identifier: GPL-3.0-or-later
#import <AppKit/AppKit.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

static emacs_value set_dark_appearance(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data) {
    BOOL dark = env->is_not_nil(env, args[0]);
    // Neomacs evaluates Lisp on a worker; AppKit owns the main thread.
    dispatch_async(dispatch_get_main_queue(), ^{
        NSApp.appearance = [NSAppearance appearanceNamed:
            dark ? NSAppearanceNameDarkAqua : NSAppearanceNameAqua];
    });
    return env->intern(env, "nil");
}

int emacs_module_init(struct emacs_runtime *runtime) {
    emacs_env *env = runtime->get_environment(runtime);
    emacs_value binding[] = {
        env->intern(env, "my/neomacs-set-dark-appearance"),
        env->make_function(env, 1, 1, set_dark_appearance,
                           "Use dark native window controls when DARK is non-nil.", NULL),
    };
    env->funcall(env, env->intern(env, "fset"), 2, binding);
    return 0;
}

lisp-labs
-----------

**lisp-labs**  is a set of sample codes whose main purpose is to experiment and test *Common Lisp* using [SBCL]

**SBCL** compilation:

First of all, create a file **customize-target-features.lisp** under *SBCL root source*, and run: *sh make.sh*

```lisp 
;; SBCL (old versions)
(lambda (features)
   (flet ((enable (x)
            (pushnew x features))
          (disable (x)
            (setf features (remove x features))))
     ;; Threading support, available only on x86/x86-64 Linux, x86 Solaris
     ;; and x86 Mac OS X (experimental).
     (enable :sb-thread)
     (enable :sb-unicode)))
```

```shell
# iconv prerequisite
git clone git@github.com:irr/libfixposix.git
cd libfixposix
git remote add upstream https://github.com/sionescu/libfixposix.git
git fetch upstream && git merge upstream/master && git push
autoreconf -i -f
./configure
sudo make install
su -c  "echo \"/usr/local/lib\" > /etc/ld.so.conf.d/libfixposix.conf"
sudo ldconfig
ldconfig -p |grep libfixposix
```

Projects:

* [practicals-1.0.3] - *Practical Common Lisp* book source code from [gigamonkeys.com]
* [web] - Basic http router
* [utils] - SBCL scripts

Dependencies
-----------

* [hunchentoot] - The Common Lisp web server formerly known as TBNL
* [rcl] - A Common Lisp Interface to R
* [cl-redis] - Redis client for Common Lisp
* [cl-json] - A JSON parser and generator in Common-Lisp
* [libfixposix] - Thin wrapper over POSIX syscalls

```lisp
(ql:quickload :quicklisp-slime-helper)
(ql:quickload :trivial-dump-core)
(ql:quickload :trivial-shell)

(ql:quickload :hunchentoot)
(ql:quickload :cl-redis)
(ql:quickload :cl-json)

(ql:quickload :lparallel)
(ql:quickload :lfarm-server)
(ql:quickload :lfarm-client)

(ql:quickload :rcl)

(ql:quickload :cl-memcached)
(ql:quickload :clsql-mysql)
(ql:quickload :clsql-sqlite3)

(ql:quickload :cl-async)
(ql:quickload :zeromq)
(ql:quickload :iconv)
```

Copyright and License
-----------
Copyright 2012 Ivan Ribeiro Rocha

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

  [SBCL]: http://www.sbcl.org
  [practicals-1.0.3]: https://github.com/irr/lisp-labs/tree/master/practicals-1.0.3
  [web]: https://github.com/irr/lisp-labs/tree/master/web
  [utils]: https://github.com/irr/lisp-labs/tree/master/utils
  [gigamonkeys.com]: http://www.gigamonkeys.com/book
  [quicklisp]: http://www.quicklisp.org
  [hunchentoot]: http://weitz.de/hunchentoot
  [rcl]: http://common-lisp.net/project/rcl
  [cl-redis]: https://github.com/vseloved/cl-redis
  [cl-json]: http://common-lisp.net/project/cl-json
  [libfixposix]: https://github.com/irr/libfixposix
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
sudo apt-get install build-essential libfixposix-dev texinfo texlive-latex-base texlive-fonts-recommended autoconf
```

Projects:

* [practicals-1.0.3] - *Practical Common Lisp* book source code from [gigamonkeys.com]
* [web] - Basic http router
* [utils] - SBCL scripts

Dependencies
-----------

* [hunchentoot] - The Common Lisp web server formerly known as TBNL
* [cl-redis] - Redis client for Common Lisp
* [cl-json] - A JSON parser and generator in Common-Lisp

```shell
curl -O http://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

```lisp
(progn
    (ql:quickload "hunchentoot") 
    (ql:quickload "cl-redis") 
    (ql:quickload "cl-json") 
    (ql:quickload "cl-memcached") 
    (ql:quickload "clsql-mysql")
    (ql:quickload "clsql-sqlite3")
    (ql:quickload "trivial-dump-core") 
    (ql:quickload "lparallel") 
    (ql:quickload :lfarm-server) 
    (ql:quickload :lfarm-client) 
    (ql:quickload "trivial-shell") 
    (ql:quickload "iconv") 
    (ql:quickload "getopt"))
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
  [practicals-1.0.3]: https://github.com/irr/cl-labs/tree/master/practicals-1.0.3
  [web]: https://github.com/irr/cl-labs/tree/master/web
  [utils]: https://github.com/irr/cl-labs/tree/master/utils
  [gigamonkeys.com]: http://www.gigamonkeys.com/book
  [quicklisp]: http://www.quicklisp.org
  [hunchentoot]: http://weitz.de/hunchentoot
  [cl-redis]: https://github.com/vseloved/cl-redis
  [cl-json]: http://common-lisp.net/project/cl-json

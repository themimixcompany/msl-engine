streams
=======


<a name="toc">Table of contents</a>
-----------------------------------

- [Overview](#overview)
- [Loading from sources](#sources)
- [Running the server](#server)
- [Building the executable](#executable)


<a name="overview">Overview</a>
-------------------------------

This repository contains information about streams, the canonical implementation of the [Nebula](https://github.com/themimixcompany/nebula) backend. Streams is written in portable Common Lisp, and is primarily ran on Steel Bank Common Lisp (SBCL).


<a name="sources">Loading from sources</a>
------------------------------------------

To load streams from sources, the minimum required software are [SBCL](http://sbcl.org) and [Git](https://git-scm.com) for Linux and Windows systems, and additionally [Homebrew](https://brew.sh/) for macOS systems.


### <a name="sourceslinux">Linux</a>

First, install SBCL and friends:

On Ubuntu systems, run:

```bash
sudo apt-get install -y sbcl git curl
```

On Fedora systems, run:

```bash
sudo yum install -y sbcl git curl
```

On Arch Linux, run:

```bash
sudo pacman -S sbcl git curl
```

Next, install [Quicklisp](https://quicklisp.org):

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (ql:quickload :cl-launch) (sb-ext:quit))'
```

Finally, fetch the dependencies and the sources of streams, itself:

```bash
mkdir -p ~/common-lisp
cd ~/common-lisp
git clone https://gitlab.common-lisp.net/asdf/asdf.git
git clone https://github.com/ebzzry/marie
git clone https://github.com/themimixcompany/streams
```


### <a name="sourcesmacos">macOS</a>

First, install Homebrew:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```

Next, install SBCL and friends:

```
brew install sbcl git
```

Next, install [Quicklisp](https://quicklisp.org):

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (ql:quickload :cl-launch) (sb-ext:quit))'
```

Finally, fetch the dependencies and the sources of streams, itself:

```bash
mkdir -p ~/common-lisp
cd ~/common-lisp
git clone https://gitlab.common-lisp.net/asdf/asdf.git
git clone https://github.com/ebzzry/marie
git clone https://github.com/themimixcompany/streams
```

### <a name="sourceswindows">Windows</a>

On Windows XP and up, download and install SBCL from [http://sbcl.org/platform-table.html](http://sbcl.org/platform-table.html).

To verify that you have succesfully install SBCL, run the following command in the Command Prompt:

```dos
sbcl --version
```

Next, install [Quicklisp](https://quicklisp.org). First, download Quicklisp from [https://beta.quicklisp.org/quicklisp.lisp](https://beta.quicklisp.org/quicklisp.lisp). Then, move
`quicklisp.lisp` from wherever it was downloaded, e.g., the Downloads folder, to `%HOMEPATH%`. In the Command Prompt go to `%HOMEDIR%`:

```dos
%HOMEDRIVE%
cd %HOMEDIR%
```

Then load `quicklisp.lisp` with the installation functions:

```dos
sbcl --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (ql:quickload :cl-launch) (sb-ext:quit))'
```

Next, download and install [Git](https://git-scm.com/download/win). To verify that you have succesfully installed Git, run the following command in the Command Prompt:

```dos
git --version
```

Next we need to install OpenSSL. Head over to the
[Win32/Win64 OpenSSL site](http://slproweb.com/products/Win32OpenSSL.html) then download and install the file [Win64 OpenSSL v1.1.1d Light](http://slproweb.com/download/Win64OpenSSL_Light-1_1_1d.exe). Restart your machine after installing it.

Finally, fetch the dependencies and the sources of streams, itself:

```dos
cd %HOMEDIR%/quicklisp/local-projects
git clone https://gitlab.common-lisp.net/asdf/asdf.git
git clone https://github.com/ebzzry/marie
git clone https://github.com/themimixcompany/streams
```

However, if you prefer to store the sources in another folder, e.g., `D:\lisp`, you may create directory symbolic links instead with [mklink](https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/mklink), to make the folder `%HOMEDIR%/quicklisp/local-projects` point to `D:\lisp`. To do so, open an Administrator Command Prompt window, then run:

```dos
deltree %HOMEDIR%/quicklisp/local-projects
d:
md lisp
cd lisp
git clone https://gitlab.common-lisp.net/asdf/asdf.git
git clone https://github.com/ebzzry/marie
git clone https://github.com/themimixcompany/streams
%HOMEDRIVE%
cd %HOMEDIR%/quicklisp
mklink /d local-projects d:\lisp
```


<a name="server">Running the server</a>
---------------------------------------

To run the server from the command line, run:

```bash
sbcl --eval '(ql:quickload :streams)' --eval '(streams:serve)'
```

Alternatively, run SBCL then evaluate the forms individually:

```bash
sbcl
* (ql:quickload :streams)
* (streams:serve)
```

Pressing C-c (Ctrl+c) will terminate the server.


<a name="executable">Building the executable</a>
------------------------------------------------

To build the standalone executable, run

```bash
sbcl --eval '(ql:quickload :streams)' --eval '(streams:build)'
```

Alternatively, run SBCL then evaluate the forms individually:

```bash
sbcl
* (ql:quickload :streams)
* (streams:build)
```

This creates the appropriate streams platform executable in the current directory.

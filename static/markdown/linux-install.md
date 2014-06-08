<table>
<tbody>
<tr class="odd">
<td align="left"><img src="http://www.haskell.org/platform/icons/ubuntu.png"> <a href="http://packages.ubuntu.com/haskell-platform">Ubuntu</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/debian.png"> <a href="http://packages.debian.org/haskell-platform">Debian</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/fedora.png"> <a href="https://apps.fedoraproject.org/packages/haskell-platform">Fedora</a></td>
</tr>
<tr class="even">
<td align="left"><img src="http://www.haskell.org/platform/icons/arch.png"> <a href="http://www.archlinux.org/packages/extra/i686/haskell-platform/">Arch Linux</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/gentoo.png"> <a href="http://www.haskell.org/haskellwiki/Gentoo/HaskellPlatform">Gentoo</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/nixos.png"> <a href="http://hydra.nixos.org/job/nixpkgs/trunk/haskellPlatform">NixOS</a></td>
</tr>
<tr class="odd">
<td align="left"><img src="http://www.haskell.org/platform/icons/openbsd.png"> <a href="http://openports.se/meta/haskell-platform">OpenBSD</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/freebsd.png"> <a href="http://www.freshports.org/devel/hs-haskell-platform/">FreeBSD</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/mint.png"> <a href="http://community.linuxmint.com/software/view/haskell-platform">Mint</a></td>
</tr>
</tbody>
</table>

<table>
<tbody>
<tr class="odd">
<td align="left"><img src="http://www.haskell.org/platform/icons/opensuse.png"> <a href="https://build.opensuse.org/project/show?project=devel:languages:haskell">openSUSE</a></td>
<td align="left"><img src="http://www.haskell.org/platform/icons/mandriva.png"> <a href="http://wiki.mandriva.com/en/Development/Tasks/Packaging/Policies/Haskell">Mandriva</a></td>
</tr>
</tbody>
</table>

<h3 id="build-from-source">Build from source</h2>
<p>Download the source tarball for Unix-like systems: here</p>
<ul>
<li><a href="download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz" onClick="javascript: pageTracker._trackPageview('/downloads/source'); ">haskell-platform-2013.2.0.0.tar.gz</a></strong> <br /><small>SHA-1: <code>8669bb5add1826c0523fb130c095fb8bf23a30ce</code></small></li>
</ul>
<p>Get and install GHC 7.6.3 prior to building the platform:</p>
<ul>
<li><a href="http://www.haskell.org/ghc/download_ghc_7_6_3">GHC 7.6.3</a></li>
</ul>
<p>Finally, unpack the Haskell Platform source tarball, and run (possibly with <tt>sudo</tt>):</p>
<pre><code>$ ./configure
$ make
$ make install</code></pre>
<p>You may pass the <tt>--prefix</tt> flag to <tt>./configure</tt> to change the default install path.</p>
<p>There is also a <a href="https://github.com/haskell/haskell-platform/blob/master/src/generic/tarball/README">README</a> file in the tarball with more detailed information on building.</p>

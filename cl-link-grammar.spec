# -*- mode: rpm-spec; fill-column: 80 -*-

%define _topdir /home/wvxvw/Projects/cl-link-grammar
%define buildroot %{_topdir}/%{name}-%{version}-root

BuildRoot: %{buildroot}
Summary: Natural Language Parser Bindings
Name: cl-link-grammar
Version: 0.0.1
Release: 1
License: LGPL2
Group: System Environment/Libraries
URL: https://github.com/wvxvw/cl-link-grammar
Source: https://github.com/wvxvw/cl-link-grammar/raw/master/%{name}-%{version}.tar.gz
Requires: sbcl, link-grammar
Provides: cl-link-grammar

%description
cl-link-grammar is a Common Lisp bindings for link-grammar library

%prep
%setup -q

%build
make

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%dir %{_datadir}/cl-link-grammar
%dir /etc/common-lisp/source-registry.conf.d/*-cl-link-grammar.conf

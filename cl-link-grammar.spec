# -*- mode: rpm-spec; fill-column: 80 -*-

%define debug_package %{nil}

Name:           cl-link-grammar
Version:        0.0.1
Release:        1%{?dist}
Summary:        Natural Language Parser Bindings

License:        LGPL2
URL:            https://github.com/wvxvw/cl-link-grammar
Source0:        https://github.com/wvxvw/cl-link-grammar/raw/master/%{name}-%{version}.tar.gz

Requires:       sbcl, link-grammar

%description
cl-link-grammar is a Common Lisp bindings for link-grammar library

%prep
%setup -q

%build

%install
%{__rm} -rf %{buildroot}
mkdir -m 755 -p %{buildroot}%{_datadir}/common-lisp/source/%{name}
for s in $(find -regex '.+\.\(lisp\|asd\|org\)$'); do
  install -D -m 644 $s %{buildroot}%{_datadir}/common-lisp/source/%{name}
done;
mkdir -m 755 -p %{buildroot}/etc/common-lisp/source-registry.conf.d
for last_conf in $(ls %{buildroot}/etc/common-lisp/source-registry.conf.d | tail -n 1); do
    for last in $(echo "${last_conf}" | grep -oP '^[0-9]+'); do
        for cl_prefix in $(echo "${last}+1" | bc); do
            echo '(:include "/usr/share/common-lisp/source/%{name}/")' > \
	            "%{buildroot}/etc/common-lisp/source-registry.conf.d/${cl_prefix}-%{name}.conf"
            install -m 644 ${cl_prefix}-%{name}.conf %{buildroot}/etc/common-lisp/source-registry.conf.d
        done;
    done;
done;

%files
%defattr(-,root,root,-)
%{_datadir}/common-lisp/source/%{name}/*
%doc



%changelog

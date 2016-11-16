sudo rpm -Uvh https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/texinfo-tex-5.1-4.el7.x86_64.rpm
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/texlive-epsf-doc-svn21461.2.7.4-38.el7.noarch.rpm
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/texlive-epsf-svn21461.2.7.4-38.el7.noarch.rpm
sudo yum localinstall *.rpm 
sudo yum update
sudo yum install –y R

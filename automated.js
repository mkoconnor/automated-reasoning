// Generated by js_of_ocaml 2.5
(function(R){"use strict";var
al=254,ak=224,ag=65535,aj=250,L="(",s=1024,af=57343,w="int_of_string",ai=512,K=")",d=128,e="",J=" : file already exists",ac=56320,ad=240,ae=2048,a=248,ab="/",ah="fd ";function
S(a,b,c){var
e=new
Array(c);for(var
d=0;d<c;d++)e[d]=a[b+d];return e}function
Q(a,b,c){var
d=String.fromCharCode;if(b==0&&c<=4096&&c==a.length)return d.apply(null,a);var
f=e;for(;0<c;b+=s,c-=s)f+=d.apply(null,S(a,b,Math.min(c,s)));return f}function
M(a){var
c=new
Array(a.l),e=a.c,d=e.length,b=0;for(;b<d;b++)c[b]=e.charCodeAt(b);for(d=a.l;b<d;b++)c[b]=0;a.c=c;a.t=4;return c}function
x(a,b,c,d,e){if(e==0)return 0;if(d==0&&(e>=c.l||c.t==2&&e>=c.c.length)){c.c=a.t==4?Q(a.c,b,e):b==0&&a.c.length==e?a.c:a.c.substr(b,e);c.t=c.c.length==c.l?0:2}else
if(c.t==2&&d==c.c.length){c.c+=a.t==4?Q(a.c,b,e):b==0&&a.c.length==e?a.c:a.c.substr(b,e);c.t=c.c.length==c.l?0:2}else{if(c.t!=4)M(c);var
g=a.c,h=c.c;if(a.t==4)for(var
f=0;f<e;f++)h[d+f]=g[b+f];else{var
i=Math.min(e,g.length-b);for(var
f=0;f<i;f++)h[d+f]=g.charCodeAt(b+f);for(;f<e;f++)h[d+f]=0}}return 0}function
bq(a,b){var
e=a.length,d=new
Array(e+1),c=0;for(;c<e;c++)d[c]=a[c];d[c]=b;return d}function
n(c,b){if(c.fun)return n(c.fun,b);var
a=c.length,d=b.length,e=a-d;if(e==0)return c.apply(null,b);else
if(e<0)return n(c.apply(null,S(b,0,a)),S(b,a,d-a));else
return function(a){return n(c,bq(b,a))}}function
bb(a,b){throw[0,a,b]}function
ar(a,b){if(b.repeat)return b.repeat(a);var
c=e,d=0;if(a==0)return c;for(;;){if(a&1)c+=b;a>>=1;if(a==0)return c;b+=b;d++;if(d==9)b.slice(0,1)}}function
o(a){if(a.t==2)a.c+=ar(a.l-a.c.length,"\0");else
a.c=Q(a.c,0,a.c.length);a.t=0}function
am(a){if(a.length<24){for(var
b=0;b<a.length;b++)if(a.charCodeAt(b)>127)return false;return true}else
return!/[^\x00-\x7f]/.test(a)}function
bm(a){for(var
l=e,f=e,i,h,j,b,c=0,k=a.length;c<k;c++){h=a.charCodeAt(c);if(h<d){for(var
g=c+1;g<k&&(h=a.charCodeAt(g))<d;g++);if(g-c>ai){f.substr(0,1);l+=f;f=e;l+=a.slice(c,g)}else
f+=a.slice(c,g);if(g==k)break;c=g}b=1;if(++c<k&&((j=a.charCodeAt(c))&-64)==d){i=j+(h<<6);if(h<ak){b=i-12416;if(b<d)b=1}else{b=2;if(++c<k&&((j=a.charCodeAt(c))&-64)==d){i=j+(i<<6);if(h<ad){b=i-925824;if(b<ae||b>=55295&&b<57344)b=2}else{b=3;if(++c<k&&((j=a.charCodeAt(c))&-64)==d&&h<245){b=j-63447168+(i<<6);if(b<65536||b>1114111)b=3}}}}}if(b<4){c-=b;f+="\ufffd"}else
if(b>ag)f+=String.fromCharCode(55232+(b>>10),ac+(b&1023));else
f+=String.fromCharCode(b);if(f.length>s){f.substr(0,1);l+=f;f=e}}return l+f}function
bl(a){switch(a.t){case
9:return a.c;default:o(a);case
0:if(am(a.c)){a.t=9;return a.c}a.t=8;case
8:return bm(a.c)}}function
g(a,b,c){this.t=a;this.c=b;this.l=c}g.prototype={toString:function(){return bl(this)}};function
c(a){return new
g(0,a,a.length)}function
P(a,b){bb(a,c(b))}var
f=[0];function
z(a){P(f[4],a)}function
a2(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
a3(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
bf(a,b){a.t&6&&o(a);b.t&6&&o(b);return a.c<b.c?-1:a.c>b.c?1:0}function
aY(a,b,c){var
e=[];for(;;){if(!(c&&a===b))if(a
instanceof
g)if(b
instanceof
g){if(a!==b){var
d=bf(a,b);if(d!=0)return d}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
f=a[0];if(f===al)f=0;if(f===aj){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===al)h=0;if(h===aj){b=b[1];continue}else
if(f!=h)return f<h?-1:1;else
switch(f){case
248:var
d=a3(a[2],b[2]);if(d!=0)return d;break;case
251:z("equal: abstract value");case
255:var
d=a2(a,b);if(d!=0)return d;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)e.push(a,b,1)}}else
return 1}else
if(b
instanceof
g||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!="number"&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(e.length==0)return 0;var
i=e.pop();b=e.pop();a=e.pop();if(i+1<a.length)e.push(a,b,i+1);a=a[i];b=b[i]}}function
aX(a,b){return aY(a,b,true)}function
k(a){if(a<0)z("String.create");return new
g(a?2:9,e,a)}function
a0(a,b,c,d){if(c>0)if(b==0&&(c>=a.l||a.t==2&&c>=a.c.length))if(d==0){a.c=e;a.t=2}else{a.c=ar(c,String.fromCharCode(d));a.t=c==a.l?0:2}else{if(a.t!=4)M(a);for(c+=b;b<c;b++)a.c[b]=d}return 0}function
p(a,b){switch(a.t&6){default:if(b>=a.c.length)return 0;case
0:return a.c.charCodeAt(b);case
4:return a.c[b]}}function
l(a){return a.l}function
ba(a){var
b=0,d=l(a),c=10,e=d>0&&p(a,0)==45?(b++,-1):1;if(b+1<d&&p(a,b)==48)switch(p(a,b+1)){case
120:case
88:c=16;b+=2;break;case
111:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,e,c]}function
ap(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
y(a){P(f[3],a)}function
a4(a){var
h=ba(a),d=h[0],i=h[1],e=h[2],g=l(a),j=-1>>>0,f=d<g?p(a,d):0,c=ap(f);if(c<0||c>=e)y(w);var
b=c;for(d++;d<g;d++){f=p(a,d);if(f==95)continue;c=ap(f);if(c<0||c>=e)break;b=e*b+c;if(b>j)y(w)}if(d!=g)y(w);b=i*b;if(e==10&&(b|0)!=b)y(w);return b|0}function
bn(a){for(var
h=e,c=h,b,j,f=0,i=a.length;f<i;f++){b=a.charCodeAt(f);if(b<d){for(var
g=f+1;g<i&&(b=a.charCodeAt(g))<d;g++);if(g-f>ai){c.substr(0,1);h+=c;c=e;h+=a.slice(f,g)}else
c+=a.slice(f,g);if(g==i)break;f=g}if(b<ae){c+=String.fromCharCode(192|b>>6);c+=String.fromCharCode(d|b&63)}else
if(b<55296||b>=af)c+=String.fromCharCode(ak|b>>12,d|b>>6&63,d|b&63);else
if(b>=56319||f+1==i||(j=a.charCodeAt(f+1))<ac||j>af)c+="\xef\xbf\xbd";else{f++;b=(b<<10)+j-56613888;c+=String.fromCharCode(ad|b>>18,d|b>>12&63,d|b>>6&63,d|b&63)}if(c.length>s){c.substr(0,1);h+=c;c=e}}return h+c}function
O(a){var
b=9;if(!am(a))b=8,a=bn(a);return new
g(b,a,a.length)}function
br(a){var
d=a.length,c=new
Array(d);for(var
b=0;b<d;b++)c[b]=a[b];return c}function
a5(a){return function(){return arguments.length>0?n(a,br(arguments)):n(a,[undefined])}}function
i(a){P(f[2],a)}function
a6(a){if(!a.opened)i("Cannot flush a closed channel");if(a.buffer==e)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=e;return 0}function
j(a){this.data=a}j.prototype={truncate:function(){this.data=k(0)}};function
aq(a){a=a
instanceof
g?a.toString():a;i(a+": No such file or directory")}var
aZ=ab;function
A(a){a=a
instanceof
g?a.toString():a;if(a.charCodeAt(0)!=47)a=aZ+a;var
d=a.split(ab),b=[];for(var
c=0;c<d.length;c++)switch(d[c]){case"..":if(b.length>1)b.pop();break;case".":break;case"":if(b.length==0)b.push(e);break;default:b.push(d[c]);break}b.orig=a;return b}function
m(){this.content={}}m.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
C=new
m();C.mk(e,new
m());function
N(a){var
b=C;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))aq(a.orig);b=b.get(a[c])}return b}function
bk(a){var
c=A(a),b=N(c);return b
instanceof
m?1:0}function
bi(a){return new
g(4,a,a.length)}function
bs(a){if(a.t!=4)M(a);return a.c}function
a1(a,b){var
f=A(a),d=C;for(var
h=0;h<f.length-1;h++){var
e=f[h];if(!d.exists(e))d.mk(e,new
m());d=d.get(e);if(!(d
instanceof
m))i(f.orig+J)}var
e=f[f.length-1];if(d.exists(e))i(f.orig+J);if(b
instanceof
m)d.mk(e,b);else
if(b
instanceof
j)d.mk(e,b);else
if(b
instanceof
g)d.mk(e,new
j(b));else
if(b
instanceof
Array)d.mk(e,new
j(bi(b)));else
if(b.toString)d.mk(e,new
j(c(b.toString())));else
z("caml_fs_register");return 0}function
bj(a){var
b=C,d=A(a),e,f;for(var
c=0;c<d.length;c++){if(b.auto){e=b.auto;f=c}if(!(b.exists&&b.exists(d[c])))return e?e(d,f):0;b=b.get(d[c])}return 1}function
u(a,b,c){if(f.fds===undefined)f.fds=new
Array();c=c?c:{};var
d={};d.file=b;d.offset=c.append?l(b.data):0;d.flags=c;f.fds[a]=d;f.fd_last_idx=a;return a}function
bt(a,b,c){var
d={};while(b){switch(b[1]){case
0:d.rdonly=1;break;case
1:d.wronly=1;break;case
2:d.append=1;break;case
3:d.create=1;break;case
4:d.truncate=1;break;case
5:d.excl=1;break;case
6:d.binary=1;break;case
7:d.text=1;break;case
8:d.nonblock=1;break}b=b[2]}var
g=a.toString(),j=A(a);if(d.rdonly&&d.wronly)i(g+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)i(g+" : flags Open_text and Open_binary are not compatible");if(bj(a)){if(bk(a))i(g+" : is a directory");if(d.create&&d.excl)i(g+J);var
h=f.fd_last_idx?f.fd_last_idx:0,e=N(j);if(d.truncate)e.truncate();return u(h+1,e,d)}else
if(d.create){var
h=f.fd_last_idx?f.fd_last_idx:0;a1(a,k(0));var
e=N(j);return u(h+1,e,d)}else
aq(a)}u(0,new
j(k(0)));u(1,new
j(k(0)));u(2,new
j(k(0)));function
a7(a){var
b=f.fds[a];if(b.flags.wronly)i(ah+a+" is writeonly");return{file:b.file,offset:b.offset,fd:a,opened:true}}function
bo(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=R.console;b&&b.error&&b.error(a)}function
bp(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=R.console;b&&b.log&&b.log(a)}var
B=new
Array();function
bd(a,b){var
h=c(b),d=l(h),g=l(a.file.data),f=a.offset;if(f+d>=g){var
e=k(f+d);x(a.file.data,0,e,0,g);x(h,0,e,f,d);a.file.data=e}a.offset+=d;return 0}function
an(a){var
b;switch(a){case
1:b=bp;break;case
2:b=bo;break;default:b=bd}var
d=f.fds[a];if(d.flags.rdonly)i(ah+a+" is readonly");var
c={file:d.file,offset:d.offset,fd:a,opened:true,buffer:e,output:b};B[c.fd]=c;return c}function
a8(){var
a=0;for(var
b
in
B)if(B[b].opened)a=[0,B[b],a];return a}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&ag)*b|0};var
ao=Math.imul;function
a_(a){return a
instanceof
Array?a[0]:a
instanceof
g?252:1e3}function
b(a,b){f[a+1]=b}var
a9={};function
aW(a){if((a.t&6)!=0)o(a);return a.c}function
bc(a,b){a9[aW(a)]=b;return 0}var
a$=0;function
t(a){a[2]=a$++;return a}function
be(){z("index out of bounds")}function
bh(a,b){if(b>>>0>=a.l)be();return p(a,b)}function
bg(a,b){a.t&6&&o(a);b.t&6&&o(b);return a.c==b.c?1:0}function
D(a,b){return 1-bg(a,b)}function
v(a,b){return a.length==1?a(b):n(a,[b])}var
T=[a,c("Failure"),-3];b(11,[a,c("Undefined_recursive_module"),-12]);b(10,[a,c("Assert_failure"),-11]);b(9,[a,c("Sys_blocked_io"),-10]);b(8,[a,c("Stack_overflow"),-9]);b(7,[a,c("Match_failure"),-8]);b(6,[a,c("Not_found"),-7]);b(5,[a,c("Division_by_zero"),-6]);b(4,[a,c("End_of_file"),-5]);b(3,[a,c("Invalid_argument"),-4]);b(2,T);b(1,[a,c("Sys_error"),-2]);b(0,[a,c("Out_of_memory"),-1]);var
as=c("Pervasives.Exit"),au=c("Array.Bottom"),av=c("Sys.Break"),aw=c("CamlinternalFormat.Type_mismatch"),az=c("Js.Error"),aI=c(e),aJ=c("+"),aK=c("*"),aL=c(L),aM=c(K),aN=c("Expected closing bracket"),aO=c("Expected an expression at end of input"),aQ=c(" + "),aR=c(K),aS=c(L),aT=c(" * "),aU=c(K),aV=c(L),aP=c("Unparsed input"),aA=[1,0],aB=c(" \t\n\r"),aD=c("()[]{},"),aE=c("~\xe2\x80\x98!@#$%^&*-+=|\\:;<>.?/"),aF=c("0123456789"),aH=c("abcdefghijklmnopqrstuvwxyz_\xe2\x80\x99ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");function
E(a){throw[0,T,a]}t([a,as,0]);function
h(a,b){var
c=l(a),e=l(b),d=k(c+e|0);x(a,0,d,0,c);x(b,0,d,c,e);return d}a7(0);an(1);an(2);t([a,au,0]);function
U(a,b){var
c=k(a);a0(c,0,a,b);return c}t([a,av,0]);t([a,aw,0]);var
V=[0,0];function
W(a){V[1]=[0,a,V[1]];return 0}var
X=R,ay=X.Array,Y=t([a,az,0]),F=[0,Y,{}],ax=a_(F)===a?F:F[0+1];bc(c("jsError"),ax);W(function(a){return a[1]===Y?[0,O(a[2].toString())]:0});W(function(a){return a
instanceof
ay?0:[0,O(a.toString())]});function
G(a){var
c=0,e=0,g=l(a);for(;;){if(g<=c){var
b=e,d=0;for(;;){if(b){var
f=[0,b[1],d],b=b[2],d=f;continue}return d}}var
h=[0,bh(a,c),e],c=c+1|0,e=h;continue}}function
q(a){var
e=G(a);return function(a){var
b=e;for(;;){if(b){var
d=b[2],c=0===aX(b[1],a)?1:0;if(c)return c;var
b=d;continue}return 0}}}var
aC=q(aB);q(aD);var
Z=q(aE),aG=q(aF),_=q(aH);function
H(a,b){if(b){var
c=b[1];if(v(a,c)){var
d=H(a,b[2]),e=d[2],f=d[1];return[0,h(U(1,c),f),e]}}return[0,aI,b]}function
$(a){var
b=H(aC,a)[2];if(b)var
c=b[1],f=v(_,c)?_:v(Z,c)?Z:function(a){return 0},d=H(f,b[2]),g=$(d[2]),i=d[1],e=[0,h(U(1,c),i),g];else
var
e=b;return e}function
I(a){var
c=aa(a),b=c[2],d=c[1];if(b)if(!D(b[1],aJ)){var
e=I(b[2]);return[0,[2,d,e[1]],e[2]]}return[0,d,b]}function
aa(a){if(a){var
e=a[1];if(D(e,aL)){var
j=a[2],c=G(e);for(;;){if(c){if(v(aG,c[1])){var
c=c[2];continue}var
k=0}else
var
k=1;var
b=k?[0,[1,a4(e)],j]:[0,[0,e],j];break}}else{var
l=I(a[2]),f=l[2];if(f)if(D(f[1],aM))var
g=0;else
var
b=[0,l[1],f[2]],g=1;else
var
g=0;if(!g)var
b=E(aN)}}else
var
b=E(aO);var
d=b[2],h=b[1];if(d)if(!D(d[1],aK)){var
i=aa(d[2]);return[0,[3,h,i[1]],i[2]]}return[0,h,d]}function
r(a,b){switch(b[0]){case
0:return b[1];case
1:return c(e+b[1]);case
2:var
g=h(aQ,r(2,b[2])),d=h(r(3,b[1]),g);return 2<a?h(aS,h(d,aR)):d;default:var
i=h(aT,r(4,b[2])),f=h(r(5,b[1]),i);return 4<a?h(aV,h(f,aU)):f}}X.simplify_string=a5(function(a){var
d=I($(G(O(a)))),e=0===d[2]?d[1]:E(aP);function
c(a){switch(a[0]){case
2:var
h=a[1];if(1===h[0]){var
i=h[1],c=a[2];switch(c[0]){case
1:return[1,i+c[1]|0];case
2:var
p=c[1];if(1===p[0])return[2,[1,i+p[1]|0],c[2]];break}if(0===i)var
o=a[2],k=1;else
var
k=0}else
var
k=0;if(k)var
e=1;else{var
n=a[2];if(1===n[0])if(0===n[1])var
o=h,e=1;else
var
e=0;else
var
e=0}if(e)return o;break;case
3:var
b=a[1];if(1===b[0]){var
j=b[1],d=a[2];switch(d[0]){case
1:return[1,ao(j,d[1])];case
3:var
t=d[1];if(1===t[0])return[3,[1,ao(j,t[1])],d[2]];break}var
u=0===j?1:0}else
var
u=0;if(u)var
f=1;else{var
q=a[2];if(1===q[0])if(0===q[1])var
f=1,l=0;else
var
l=1;else
var
l=1;if(l){if(1===b[0])if(1===b[1])var
s=a[2],m=1;else
var
m=0;else
var
m=0;if(m)var
g=1;else{var
r=a[2];if(1===r[0])if(1===r[1])var
s=b,g=1;else
var
f=0,g=0;else
var
f=0,g=0}if(g)return s}}if(f)return aA;break}return a}function
b(a){switch(a[0]){case
2:var
d=b(a[2]);return c([2,b(a[1]),d]);case
3:var
e=b(a[2]);return c([3,b(a[1]),e]);default:return a}}return r(0,b(e)).toString()});function
at(a){var
b=a;for(;;){if(b){var
c=b[2],d=b[1];try{a6(d)}catch(f){}var
b=c;continue}return 0}}at(a8(0));return}(function(){return this}()));

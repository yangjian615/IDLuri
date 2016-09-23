; docformat = 'rst'
;
; NAME:
;       MrParseURL
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Parse a URL
;
;   Parts:
;       URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
;       hier-part   = "//" authority path-abempty
;                     / path-absolute
;                     / path-rootless
;                     / path-empty
;
;   Regular Expresssion:
;       ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
;        12            3  4          5       6  7        8 9
;
;          scheme    = $2
;          authority = $4
;          path      = $5
;          query     = $7 
;          fragment  = $9
;
;   References:
;       Section 3 of http://www.ietf.org/rfc/rfc3986.txt
;       https://url.spec.whatwg.org/
;       2.4 Parsing a URL, http://www.freesoft.org/CIE/RFC/1808/6.htm
;
; :Params:
;       URL:            in, optional, type=string/strarr
;                       URL to be parsed.
;
; :Keywords:
;       HOST:           out, optional, type=string/strarr
;                       Name of the remote server. Can be an IP address.
;       PASSWORD:       out, optional, type=string/strarr
;                       Password used when authenitcating with a remote server.
;       PATH:           out, optional, type=string/strarr
;                       Full path to the network resource.
;       PORT:           out, optional, type=string/strarr
;                       Value of the TCP/IP port that the remote server monitors for
;                           incoming requests.
;       QUERY:          out, optional, type=string/strarr
;                       Portion of the URL following the "?" character.
;       SCHEME:         out, optional, type=string/strarr
;                       Name of the protocol used to access the remote server.
;                           Values are {http | https | ftp | ftps}.
;       USERNAME:       out, optional, type=string/strarr
;                       Username used when authenticating with a remote server.
;
;-
pro MrParseURL, url, $
AUTHORITY=authority, $
FRAGMENT=fragment, $
HOST=host, $
PASSWORD=password, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme, $
USERINFO=userinfo, $
USERNAME=username
	compile_opt strictarr
	on_error, 2

	;Allocate memory
	nURL      = n_elements(url)
	authority = strarr(nURL)
	fragment  = strarr(nURL)
	host      = strarr(nURL)
	path      = strarr(nURL)
	password  = strarr(nURL)
	port      = strarr(nURL)
	query     = strarr(nURL)
	scheme    = strarr(nURL)
	userinfo  = strarr(nURL)
	username  = strarr(nURL)
	
;-----------------------------------------------------
; Parse URI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	; Syntax Components
	;   The scheme and path components are required, though the path may be
	;   empty (no characters).  When authority is present, the path must
	;   either be empty or begin with a slash ("/") character.  When
	;   authority is not present, the path cannot begin with two slash
	;   characters ("//").  These restrictions result in five different ABNF
	;   rules for a path (Section 3.3), only one of which will match any
	;   given URI reference.
	;
	;      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
	;
	;      hier-part   = "//" authority path-abempty
	;                  / path-absolute
	;                  / path-rootless
	;                  / path-empty
	;
	; Example
	;         foo://example.com:8042/over/there?name=ferret#nose
	;         \_/   \______________/\_________/ \_________/ \__/
	;          |           |            |            |        |
	;       scheme     authority       path        query   fragment
	;          |   _____________________|__
	;         / \ /                        \
	;         urn:example:animal:ferret:nose
	;
	
	;
	;URI
	;   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
	;   hier-part   = "//" authority path-abempty
	;                 / path-absolute
	;                 / path-rootless
	;                 / path-empty
	;
	;   ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
	;   12            3  4          5       6  7        8 9
	;
	;      scheme    = $2
	;      authority = $4
	;      path      = $5
	;      query     = $7ssh 
	;      fragment  = $9
	;
	parts = stregex(url, '^(([^:/?#]+):)?' + $  ;Scheme
	                     '(//([^/?#]*))?'  + $  ;Authority
	                     '([^?#]*)'        + $  ;Path
	                     '(\?([^#]*))?'    + $  ;Query
	                     '(#(.*))?',         $  ;Fragment
	                     /SUBEXP, /EXTRACT)
	
	;Did we find a match?
	iFail = where(parts[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then $
		message, 'Invalid URL: ["' + strjoin(url[iFail], '", "') + '"]', /INFORMATIONAL

	;Extract the parts
	scheme    = reform(parts[2,iPass])
	authority = reform(parts[4,iPass])
	path      = reform(parts[5,iPass])
	query     = reform(parts[7,iPass])
	fragment  = reform(parts[9,iPass])
	
	
;-----------------------------------------------------
; Scheme \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Scheme
	;   Scheme names consist of a sequence of characters beginning with a
	;   letter and followed by any combination of letters, digits, plus
	;   ("+"), period ("."), or hyphen ("-").
	;
	;   scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
	
	
;-----------------------------------------------------
; Athority \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Authority
	;   The authority component is preceded by a double slash ("//") and is
	;   terminated by the next slash ("/"), question mark ("?"), or number
	;   sign ("#") character, or by the end of the URI.
	;
	;   authority   = [ userinfo "@" ] host [ ":" port ]

	;
	; User Information
	;   The user information, if present, is followed by a
	;   commercial at-sign ("@") that delimits it from the host.
	;
	;   userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
	;
	;   pct-encoded = "%" HEXDIG HEXDIG
	;   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
	;   reserved    = gen-delims / sub-delims
	;   gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
	;   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
	;                 / "*" / "+" / "," / ";" / "="
	;
	hexdig      = '0-9a-fA-F'
	pct_encoded = '%' + hexdig
	unreserved  = 'a-zA-Z0-9._-~'
	gen_delims  = ':/?#[]@'
	sub_delims  = "!$&'()*+,;="
	reserved    = gen_delims + sub_delims
	
	;
	; Host Information
	;   The host subcomponent of authority is identified by an IP literal
	;   encapsulated within square brackets, an IPv4 address in dotted-
	;   decimal form, or a registered name.
	;
	;   host       = IP-literal / IPv4address / reg-name
	;   IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
	;   IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
	;   reg-name    = *( unreserved / pct-encoded / sub-delims )
	;
	; IPv4 Address
	;   See MrIsIPv4
	;
	; IPv6 Address
	;   See MrIsIPv6
	;
	; Registered Name
	;   A sequence of domain labels separated by ".",
	;   each domain label starting and ending with an alphanumeric character
	;   and possibly also containing "-" characters.  The rightmost domain
	;   label of a fully qualified domain name in DNS may be followed by a
	;   single "." and should be if it is necessary to distinguish between
	;   the complete domain name and some local domain.
	;
	;   reg-name    = *( unreserved / pct-encoded / sub-delims )
	;
	
	;
	; Port
	;   The port subcomponent of authority is designated by an optional port
	;   number in decimal following the host and delimited from it by a
	;   single colon (":") character.
	;
	;     port        = *DIGIT
	;

	;User Info
	parts    = stregex(authority, '(([^@]*)@)?([A-Za-z0-9.-]+)(:([0-9]+))?$', /SUBEXP, /EXTRACT)
	userinfo = reform(parts[2,*])
	host     = reform(parts[3,*])
	port     = reform(parts[5,*])

	;Check for username and password
	component = '[' + unreserved + sub_delims + ']*'
	userpass = stregex(userinfo, '^(' + component + '):(' + component + ')$', /SUBEXP, /EXTRACT)
	username = reform(userpass[1,*])
	password = reform(userpass[2,*])
	
;-----------------------------------------------------
; Path \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	; Path
	;   The path is terminated by the first question mark ("?") or
	;   number sign ("#") character, or by the end of the URI.
	;
	;
	;      path          = path-abempty    ; begins with "/" or is empty
	;                    / path-absolute   ; begins with "/" but not "//"
	;                    / path-noscheme   ; begins with a non-colon segment
	;                    / path-rootless   ; begins with a segment
	;                    / path-empty      ; zero characters
	;
	;      path-abempty  = *( "/" segment )
	;      path-absolute = "/" [ segment-nz *( "/" segment ) ]
	;      path-noscheme = segment-nz-nc *( "/" segment )
	;      path-rootless = segment-nz *( "/" segment )
	;      path-empty    = 0<pchar>
	;      segment       = *pchar
	;      segment-nz    = 1*pchar
	;      segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
	;                    ; non-zero-length segment without any colon ":"
	;
	;      pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
	;
;-----------------------------------------------------
; Query \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	; Query
	;   The query component is indicated by the first question
	;   mark ("?") character and terminated by a number sign ("#") character
	;   or by the end of the URI.
	;
	;     query       = *( pchar / "/" / "?" )
	
;-----------------------------------------------------
; Fragment \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	; Fragment
	;   A fragment identifier component is indicated by the presence of a
	;   number sign ("#") character and terminated by the end of the URI.
	;
	;      fragment    = *( pchar / "/" / "?" )
	;



	;Return scalars?
	if nURL eq 1 then begin
		authority = authority[0]
		fragment  = fragment[0]
		host      = host[0]
		password  = password[0]
		path      = path[0]
		port      = port[0]
		query     = query[0]
		scheme    = scheme[0]
		userinfo  = userinfo[0]
		username  = username[0]
	endif
end


;Example URIs
uris = [ 'ftp://ftp.is.co.za/rfc/rfc1808.txt', $
        'http://www.ietf.org/rfc/rfc2396.txt', $
        'ldap://[2001:db8::7]/c=GB?objectClass?one', $
        'mailto:John.Doe@example.com', $
        'news:comp.infosystems.www.servers.unix', $
        'tel:+1-816-555-1212', $
        'telnet://192.0.2.16:80/', $
        'urn:oasis:names:specification:docbook:dtd:xml:4.1.2']

;Parse the URIs
MrParseURL, uris, AUTHORITY=auth, FRAGMENT=frag, HOST=host, PASSWORD=pass, PATH=path, $
                  PORT=port, QUERY=query, SCHEME=scheme, USERNAME=user

;Length for display purposes
auth_len = strtrim(max(strlen(auth)),   2)
frag_len = strtrim(max(strlen(frag)),   2)
path_len = strtrim(max(strlen(path)),   2)
qry_len  = strtrim(max(strlen(query)),  2)
schm_len = strtrim(max(strlen(scheme)), 2) 

;Formats
auth_fmt = '%-' + auth_len + 's'
frag_fmt = '%-' + frag_len + 's'
path_fmt = '%-' + path_len + 's'
qry_fmt  = '%-' + qry_len  + 's'
schm_fmt = '%'  + schm_len + 's'

;Print URLS
print, ''
print, 'URIs'
print, '  ' + transpose(uris)
print, ''

;Output format
fmt = '(%"' + schm_fmt + '   ' + auth_fmt + '   ' + path_fmt + '   ' + qry_fmt + '   ' + frag_fmt + '")'
print, FORMAT=fmt, 'SCHEME', 'AUTHORITY', 'PATH', 'QUERY'
for i = 0, n_elements(scheme) - 1 $
	do print, FORMAT=fmt, scheme[i], auth[i], path[i], query[i], frag[i]
print, ''

end
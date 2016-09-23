; docformat = 'rst'
;
; NAME:
;       MrURI__Define
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
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
;+
;   Provide a GUI, similar to Dialog_Pickfile, for downloading files from the internet.
;
; :See Also:
;   MrParseURL.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2013-06-28  -   Written by Matthew Argall.
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrURI::_OverloadPrint
	compile_opt idl2
	on_error, 2
	
	;Create strings
	fragment = string('  Fragment', '=', self.fragment, FORMAT='(a-26, a-2, a0)')
	host     = string('  Host',     '=', self.host,     FORMAT='(a-26, a-2, a0)')
	path     = string('  Path',     '=', self.path,     FORMAT='(a-26, a-2, a0)')
	query    = string('  Query',    '=', self.query,    FORMAT='(a-26, a-2, a0)')
	scheme   = string('  Scheme',   '=', self.scheme,   FORMAT='(a-26, a-2, a0)')
	username = string('  Username', '=', self.username, FORMAT='(a-26, a-2, a0)')

	;Output array
	outStr = [ [ fragment ], $
	           [ host     ], $
	           [ path     ], $
	           [ query    ], $
	           [ scheme   ], $
	           [ username ] $
	         ]

	;Print the array
	return, outStr
end


;+
;   Chance directories.
;
; :Examples:
;   Change URLs relative to the current URL::
;       IDL> wg = obj_new('MrURI')
;       IDL> wg.SetURL, 'http://emfisis.physics.uiowa.edu/Flight/'
;       IDL> wg.cd, './L3/2013/'   
;       IDL> wg.PWD
;           http://emfisis.physics.uiowa.edu/Flight/L3/2013
;
;   Move up two directories::
;       IDL> wg.cd, '../../'
;       IDL> wg.PWD
;           http://emfisis.physics.uiowa.edu/Flight
;
;   Change to a different host
;       IDL> wg -> CD, 'http://www.google.com/' 
;       IDL> print, wg.GetURL()
;           http://www.google.com/
;
;   Change to a website that requires a log-in
;       IDL> wg -> CD, 'http://mmsdata.sr.unh.edu/'
;   
;
; :Params:
;       DESTINATION:    in, optional, type=integer, default='*'
;                       Change to this directory. For absolute paths, which start
;                           with "/", this is equivalent to setting the URL_PATH property.
;                           Relative paths will be appended to the current URL.
;                           The ".." and "./" symbols are recognized.
;-
pro MrURI::CD, destination, $
COUNT=count, $
ERROR=the_error
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if arg_present(the_error) eq 0 then self -> Error_Handler
		return
	endif
	
	pathsep = '/'

	;Full URL?
	if stregex(destination, '^(file|http|https|ftp|ftps)', /BOOLEAN) then begin
		uri = destination

	;Absolute path
	endif else if stregex(destination, '^' + pathsep, /BOOLEAN) then begin
		uri = self -> BuildURI( FRAGMENT = self.fragment, $
		                        HOST     = self.host, $
		                        PASSWORD = self.password, $
		                        PATH     =      destination, $
		                        PORT     = self.port, $
		                        QUERY    = self.query, $
		                        SCHEME   = self.scheme, $
		                        USERNAME = self.username )

	;Up one directory: ".."
	;   - Look for two dots before looking for one dot.
	endif else if stregex(destination, '^\.\.' + pathsep + '?', /BOOLEAN) then begin
		self -> CD_UpOneDir
		destOut = stregex(destination, '^\.\.(' + pathsep + '(.*))?', /EXTRACT, /SUBEXP)
		destOut = destOut[2]
		self -> CD, destOut
		return

	;Relative to the current directory: "./"
	endif else if stregex(destination, '^\.' + pathsep + '?', /BOOLEAN) then begin
		relPath = stregex(destination, '^\.(' + pathsep + '(.*))?', /SUBEXP, /EXTRACT)
		relPath = destOut[2]
		absPath = relPath eq '' ? '' : self -> Path_Append(relPath, ROOT=self.path)
		
		;Build the new URI
		uri = self -> BuildURI( FRAGMENT = self.fragment, $
		                        HOST     = self.host, $
		                        PASSWORD = self.password, $
		                        PATH     =      absPath, $
		                        PORT     = self.port, $
		                        QUERY    = self.query, $
		                        SCHEME   = self.scheme, $
		                        USERNAME = self.username )
		
	
	;Relative path.
	endif else if stregex(destination, '^[^/]', /BOOLEAN) then begin
		;Get the absolute path
		absPath = self -> Path_Append(destination, ROOT=self.path)
		
		;Build the new URI
		uri = self -> BuildURI( FRAGMENT = self.fragment, $
		                        HOST     = self.host, $
		                        PASSWORD = self.password, $
		                        PATH     =      absPath, $
		                        PORT     = self.port, $
		                        QUERY    = self.query, $
		                        SCHEME   = self.scheme, $
		                        USERNAME = self.username )
	endif else begin
		uri = destination
	endelse

	;Change directories
	if uri ne '' then self -> SetURI, uri
end


;+
;   Move up one directory.
;-
pro MrURI::CD_UpOneDir
	compile_opt idl2
	on_error, 2

	;Remove the last piece from the directory tree
	newPath = self -> Path_DirName(self.path)
	
	;Build the URL
	newURI = self -> BuildURI( FRAGMENT = self.fragment, $
	                           HOST     = self.host, $
	                           PASSWORD = self.password, $
	                           PATH     = newPath, $
	                           PORT     = self.port, $
	                           QUERY    = self.query, $
	                           SCHEME   = self.scheme, $
	                           USERNAME = self.username )
	
	;Set the URI
	self -> SetURI, newURI
end


;+
;   Get the URL
;
; :Params:
;       URI:            out, required, type=string
;                       The URI to be made the current uri.
;-
function MrURI::Get, uri
	compile_opt idl2
	on_error, 2
	
	message, 'This method must be over-ridden by a subclass.'
end


;+
;   Get the directory listings.
;
; :Private:
;-
function MrURI::GetDirList
	compile_opt idl2
	on_error, 2
	
	message, 'The GetDirList method must be over-ridden by a subclass.'
	
	return, -1
end


;+
;   Get the URL
;
; :Params:
;       URI:            out, required, type=string
;                       The URI to be made the current uri.
;-
function MrURI::GetURI
	compile_opt idl2
	on_error, 2
	
	;Form the URI
	uri = self -> BuildURI( FRAGMENT = self.fragment, $
	                        HOST     = self.host, $
	                        PASSWORD = self.password, $
	                        PATH     = self.path, $
	                        PORT     = self.port, $
	                        QUERY    = self.query, $
	                        SCHEME   = self.scheme, $
	                        USERNAME = self.username )
	
	return, uri
end


;+
;   Get the URL
;
; :Params:
;       URI:            out, required, type=string
;                       The URI to be made the current uri.
;-
pro MrURI::Error_Handler
	compile_opt idl2
	on_error, 2
	
	;Generate an error
	MrPrintF, 'LogErr'
end


;+
;   Build a URI
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
; :Returns:
;       URL:            in, optional, type=string/strarr
;                       URL to be parsed.
;-
function MrURI::BuildURI, $
FRAGMENT=fragment, $
HOST=host, $
PASSWORD=password, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme, $
USERNAME=username
	compile_opt idl2
	on_error, 2

	;Allocate memory
	if n_elements(authority) eq 0 then authority = ''
	if n_elements(fragment)  eq 0 then fragment  = ''
	if n_elements(host)      eq 0 then host      = ''
	if n_elements(password)  eq 0 then password  = ''
	if n_elements(path)      eq 0 then path      = ''
	if n_elements(port)      eq 0 then port      = ''
	if n_elements(query)     eq 0 then query     = ''
	if n_elements(scheme)    eq 0 then scheme    = ''
	if n_elements(userinfo)  eq 0 then userinfo  = ''
	if n_elements(username)  eq 0 then username  = ''
	
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
	
	uri = scheme + '://'
	if username ne '' || password ne '' then uri += username + ':' + password + '@'
	if host ne '' then uri += host
	if port ne '' then uri += ':' + port
	
	;If path starts with "/", no need to introduce one.
	if strmid(path, 0, 1) ne '/' then uri += '/'
	
	if path     ne '' then uri += path
	if query    ne '' then uri += '?' + query
	if fragment ne '' then uri += '#' + fragment

	return, uri
end


;+
;    Print the list of files in the current directory.
;
; :Params:
;       SEARCHSTR:      in, optional, type=string, default=''
;                       A pattern to match against files in the current directory.
;                           Files and directoires matching this search string will
;                           be returned. If the search string is preceeded by a
;                           directory path, the search will take place in that
;                           directory instead of the current directory.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of files found.
;       DIRECTORY:      in, optional, type=boolean, default=0
;                       Return only directories.
;       OUTPUT:         out, optional, type=string/strarr
;                       A named variable into which the directory contents are returned.
;                           If present, the contents are not printed to the display.
;       REGEX:          in, optional, type=boolean, default=0
;                       If set, StRegExp will be used with `PATTERN` instead of StrMatch.
;-
pro MrURI::LS, srchstr, $
COUNT=count, $
ERROR=the_error, $
DIRECTORY=directory, $
OUTPUT=output, $
REGEX=regex, $
SORT=tf_sort
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_regex = keyword_set(regex)
	tf_sort  = keyword_set(tf_sort)
	output   = ''

	;Get directory contents
	dirList = self -> GetDirList(COUNT=count)

	;Weed out results
	if count gt 0 && n_elements(srchstr) gt 0 then begin
		if keyword_set(regex) $
			then tf_match = stregex(dirList, srchstr, /BOOLEAN) $
			else tf_match = strmatch(dirList, srchstr)
		
		iMatch = where(tf_match, count)
		if count eq 0 $
			then dirList = '' $
			else dirList = reform(dirList[iMatch])
	endif

	;Output or display?
	if arg_present(output) then begin
		output = temporary(dirList)
	endif else begin
		;Tranpose does not like scalars.
		if count le 1 then dirList = [dirList]
		print, '  ' + transpose(dirList)
	endelse
end


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
;   See Also:
;       MrParseURI
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
pro MrURI::ParseURI, uri, $
AUTHORITY=authority, $
FRAGMENT=fragment, $
FIELD_VALUES=field_values, $
HOST=host, $
PASSWORD=password, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme, $
USERINFO=userinfo, $
USERNAME=username
	compile_opt idl2
	on_error, 2

	;Allocate memory
	nURI      = n_elements(uri)
	authority = strarr(nURI)
	fragment  = strarr(nURI)
	host      = strarr(nURI)
	path      = strarr(nURI)
	password  = strarr(nURI)
	port      = strarr(nURI)
	query     = strarr(nURI)
	scheme    = strarr(nURI)
	userinfo  = strarr(nURI)
	username  = strarr(nURI)
	
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
	parts = stregex(uri, '^(([^:/?#]+):)?' + $  ;Scheme
	                     '(//([^/?#]*))?'  + $  ;Authority
	                     '([^?#]*)'        + $  ;Path
	                     '(\?([^#]*))?'    + $  ;Query
	                     '(#(.*))?',         $  ;Fragment
	                     /SUBEXP, /EXTRACT)
	
	;Did we find a match?
	iFail = where(parts[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then $
		message, 'Invalid URI: "' + strjoin(uri[iFail], '", "') + '".', /INFORMATIONAL

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
	;
	if arg_present(field_values) && ~array_equal(query, '') then begin
		if n_elements(query) gt 1 then message, 'QUERY is an array. Cannot handle this at the moment.'
		
		;Split by fields
		parts = strsplit(query, '&', /EXTRACT, COUNT=nParts)
		
		;Split into field and value
		parts  = stregex(parts, '([^=]+)=([^&]+)', /SUBEXP, /EXTRACT)
		fields = idl_validname(reform(parts[1,*]), /CONVERT_ALL)
		values = reform(parts[2,*])
		
		;Create the structure
		field_values = create_struct( fields[0], values[0] )
		for i = 1, nParts-1 do field_values = create_struct( field_values, fields[i], values[i] )
	endif
	
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
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Return scalars?
	if nURI eq 1 then begin
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


;+
;   Parse the directory, file name, and file extension from the path.
;-
function MrURI::Path_Append, piece, $
ROOT=root
	compile_opt idl2
	on_error, 2
	
	;Number of pieces given
	nPieces = n_elements(piece)
	newPath = strarr(nPieces)

	;Default to the present working directory
	if n_elements(root) eq 0 then root = self -> GetURI()

	;Remove all leading path separators from PIECE
	;   - For StrMid() act how we want, POS1 must be 1xN
	pos1 = stregex(piece, '^/+')
	if nPieces gt 1 then pos1 = transpose(pos1)
	
	;Trim '/' from the pieces
	iAbs = where(pos1 ne -1, nAbs, COMPLEMENT=iRel, NCOMPLEMENT=nRel)
	if nAbs gt 0 then newPath[iAbs] = strmid(piece[iAbs], pos1+1)
	if nRel gt 0 then newPath[iRel] = piece[iRel]

	;Remove all trailing path separators from ROOT.
	pos0 = stregex(root, '/+$')
	if pos0 eq -1 $
		then newPath = root + '/' + newPath $
		else newPath = strmid(root, 0, pos0) + '/' + newPath

	;Return the file name
	if nAbs + nRel eq 1 then newPath = newPath[0]
	return, newPath
end


;+
;   Extract the base name from the path
;-
function MrURI::Path_BaseName, path, $
EXTENSION=extension
	compile_opt idl2
	on_error, 2
	
	nPath = n_elements(path)
	
	;Search for the trailing '/'
	;   - StrMid() requires a 1xN array to return desired results
	pos = strpos(path, '/', /REVERSE_SEARCH)
	if nPath gt 1 then pos = transpose(pos)
	basename = strarr(nPath)
	
	;Parse path
	;   - Extract everything after the last '/'
	;   - If there is no '/', return the path
	iRemove = where(pos ne -1, nRemove, COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)
	if nRemove gt 0 then basename[iRemove] = strmid(path[iRemove], pos+1)
	if nKeep   gt 0 then basename[iKeep]   = path[iKeep]
	
	;Find the extansion
	if arg_present(extension) then begin
		pos = strpos(basename, '.', /REVERSE_SEARCH)
		extension = strarr(nPath)

		;Parse path
		;   - Extract everything after the last '/'
		;   - If there is no '/', return the empty string
		iExt = where(pos ne -1, nExt)
		if nExt gt 0 then extension[iExt] = strmid(basename[iExt], pos+1)
		if nPath eq 1 then extension = extension[0]
	endif
	
	
	if nPath eq 1 then basename = basename[0]
	return, basename
end


;+
;   Extract the directory name from the path.
;-
function MrURI::Path_DirName, path
	compile_opt idl2
	on_error, 2
	
	nPath = n_elements(path)
	
	;Search for the trailing '/'
	;   - StrMid() requires a 1xN array to return desired results
	pos = strpos(path, '/', /REVERSE_SEARCH)
	if nPath gt 1 then pos = transpose(pos)
	dirname = strarr(nPath)
	
	;Parse path
	;   - Extract everything before the last '/'
	;   - If there is no '/', return the empty string
	iRemove = where(pos ne -1, nRemove)
	if nRemove gt 0 then dirname[iRemove] = strmid(path, 0, pos)
	
	if nPath eq 1 then dirname = dirname[0]
	return, dirname
end


;+
;   Parse the directory, file name, and file extension from the path.
;-
function MrURI::Path_RootName, path, $
DIRECTORY=directory, $
EXTENSION=extension
	compile_opt idl2
	on_error, 2

	;Default output
	file      = ''
	directory = ''
	extension = ''
	
	;If no path was given, get the current directory before returning
	if n_elements(path) eq 0 || path eq '' then begin
		directory = self -> GetPWD()
		if strmid(directory, 0, 1, /REVERSE_OFFSET) ne '/' then directory += '/'
		return, file
	endif
	
	;Path is a directory
	if strmid(path, 0, 1, /REVERSE_OFFSET) eq '/' then begin
		directory = path
		return, file
	endif
	
	;Path is directory + filename
	;   - PATH is always absolute, so replace the leading "/"
	parts    = strsplit(path, '/', COUNT=nParts, /EXTRACT)
	parts[0] = '/' + parts[0]
	
	;Put parts together
	case nParts of
		1: file = parts[0]
			
		;File and root directory exist
		2: begin
			directory = parts[0] + '/'
			file      = parts[1]
		endcase
		
		;File and directory structure
		else: begin
			file      = parts[nParts-1]
			directory = strjoin(parts[0:nParts-2], '/') + '/'
		endcase
	endcase
	
	
	;Extract extension.
	subparts = strsplit(file, '.', COUNT=nSubParts, /EXTRACT)
	case nSubParts of
		1: file = subparts[0]
		2: begin
			file      = subparts[0]
			extension = subparts[1]
		endcase
		else: begin
			file      = strjoin(parts[0:nSubParts-2], '.')
			extension = subparts[nSubParts-1]
		endcase
	endcase
	
	;Return the file name
	return, file
end


;+
;   Print the present working directory to the display.
;
; :Examples:
;    Create a new MrURI object and print the current directory
;        IDL> oWeb = Obj_New('MrURI')
;        IDL> oWeb -> PWD
;           /Users/username
;-
pro MrURI::PWD
	print, self -> GetURI()
end


;+
;   Resolve the URI.
;
; :Params:
;       URI:            in, required, type=string
;                       The URL to be made the current url\i.
;-
function MrURI::ResolveURI, uri
	compile_opt idl2
	on_error, 2

	;Parse the URI
	self     -> ParseURI, uri, SCHEME=scheme
	jResolve  = where(scheme eq '', nResolve, COMPLEMENT=iFull, NCOMPLEMENT=nFull)
	
	;Fully resolved paths
	outURI  = strarr(nFull+nResolve)
	outURI[iFull] = uri[iFull]
	

	;Resolve incomplete URLs
	if nResolve gt 0 then begin
		;Allocate memory
		absPath = strarr(nResolve)
	
		;If the path is preceeded by "/", then it is an absolute path
		jAbsolute = where(strmid(outURI[jResolve], 0, 1) eq '/', nAbsolute, $
		                  COMPLEMENT=jRelative, NCOMPLEMENT=nRelative)
	
		;Absolute paths
		if nAbsolute gt 0 then begin
			iAbs          = jResolve[jAbsolute]
			absPath[iAbs] = uri[iAbs]
		endif
	
		;Relative paths
		if nRelative gt 0 then begin
			iRel          = jResolve[jRelative]
			absPath[iRel] = self -> Path_Append(uri[iRel], ROOT=self.path)
		endif
		
		;Create fully qualified URIs
		outURI[jResolve] = self -> BuildURI( FRAGMENT = self.fragment, $
		                                     HOST     = self.host, $
		                                     PASSWORD = self.password, $
		                                     PATH     =      absPath, $
		                                     PORT     = self.port, $
		                                     QUERY    = self.query, $
		                                     SCHEME   = self.scheme, $
		                                     USERNAME = self.username )
	endif

	return, outURI
end


;+
;   Set the URI.
;
; :Params:
;       URI:            in, required, type=string
;                       The URL to be made the current uri.
;-
pro MrURI::SetURI, uri
	compile_opt idl2
	on_error, 2

	message, 'MrURI::SetURI must be over-ridden by a subclass.'
end


;+
;   Able to recursively find files from a file URI, given a URI or URI pattern.
;   URI patterns can have any token recognized by MrTokens.pro.
;
; :Params:
;       URL:            in, required, type=string
;                       URI or URI pattern to be found.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of links found.
;       ERROR:          out, optional, type=integer
;                       Variable to recieve the error code. 0 indicates no error. If
;                           present, the error message will be supressed.
;-
function MrURI::Search, uri, $
COUNT=count, $
ERROR=the_error
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if arg_present(the_error) eq 0 then MrPrintF, 'LogErr'
		return, ''
	endif

	;Default
	if n_elements(uri) eq 0 then uri = self -> GetURI()
	purl = self -> GetURI()

;---------------------------------------------------------------------
; Find First Token ///////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Make sure we are at the proper host
	;   - Use ::SetURL so that log-ins are performed when needed
	fullURI = self -> ResolveURI(uri)
	self -> ParseURI, fullURI, SCHEME=uriScheme, HOST=uriHost, PATH=uriPath
	if (uriScheme ne self.scheme) || (uriHost ne self.host) $
		then self -> SetURI, uriScheme + '://' + uriHost

	;Indicate uriPath is absolute. Split the path
	pos        = strsplit(uriPath, '/', COUNT=nParts, LENGTH=len)
	path_parts = strmid(uriPath, pos, len)

	;Find the largest path not containing tokens. There is one extra character
	;for each "/" that was split off, plus 2 for the leading and trailing "/"
	iFirstToken = min(where(strpos(path_parts, '%') ne -1))

	;Extract the root and subpattern.
	;   root       = Leading path segments without tokens. 
	;   subPattern = First path segment containing tokens
	case iFirstToken of
		;No tokens
		-1: begin
			root       = self -> Path_DirName(uriPath)
			subpattern = self -> Path_BaseName(uriPath)
		endcase
	
		;First segment contains a token
		0: begin
			root       = '/'
			subpattern = path_parts[0]
		endcase
	
		;All other cases.
		else: begin
			root       = strmid(uriPath, 0, pos[iFirstToken])
			subPattern = path_parts[iFirstToken]
			subPattern = MrTokens_ToRegex(subPattern)
		endelse
	endcase

;---------------------------------------------------------------------
; Search for Match ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Change directories to ROOT and search for PATTERN
	self -> CD, root
	self -> LS, subPattern, REGEX=(iFirstToken ne -1), COUNT=count, OUTPUT=linkOut, ERROR=the_error
	if the_error ne 0 then message, /REISSUE_LAST
	if count eq 0 then begin
		self -> CD, purl
		return, ''
	endif

	;
	; We need to exclude the following types of links:
	;   - External links
	;   - Links that have already been found
	;   - Links that move up the directory sturcture (e.g. .. or other absolute paths)
	;
	; As a first stupid case, take only relative paths.
	;
	
	;Find absolute paths
	iAbs = where(stregex(linkOut, '^/', /BOOLEAN), nAbs, COMPLEMENT=iRel, NCOMPLEMENT=count)
	if nAbs gt 0 then begin
		MrPrintF, 'LogWarn', nAbs, FORMAT='(%"Skipping %i absolute path.")'
		MrPrintF, 'LogText', '    ' + linkOut[iAbs]
	endif
	
	;Exclude absolute paths
	if count gt 0 $
		then linkOut = linkOut[iRel] $
		else return, ''

	;Remainder of the directory to be parsed
	if (iFirstToken eq nParts-1) || (iFirstToken eq -1) $
		then remainder = '' $
		else remainder = strjoin(path_parts[iFirstToken+1:nParts-1], '/')

;---------------------------------------------------------------------
; Parse Next Piece ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	if remainder ne '' then begin
		;Step through each match
		for i = 0, count - 1 do begin
			;Substitute the match for its regular expression and search again
			;   root + regex + remainder ---> root + match + remainder
			nextLink  = self -> Path_Append(linkOut[i], ROOT=root)
			nextLink  = self -> Path_Append(remainder, ROOT=nextLink)
			temp_tree = self -> Search(nextLink, COUNT=nFound)

			;Gather all of the complete file paths.
			if nFound gt 0 then begin
				if n_elements(tree) eq 0 $
					then tree = temp_tree $
					else tree = [tree, temp_tree]
			endif
		endfor

;---------------------------------------------------------------------
; Last Element ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		;Form the complete file path
		;   root + regex ----> root + match
		tree = self -> Path_Append(linkOut, ROOT=root)
	endelse

	;Return to the original directory
	self -> SetURI, purl

	;Count the results. If no matches were found, return
	;the empty string. Otherwise, append the host to the url path
	count = n_elements(tree)
	if count eq 0 then begin
		tree = '' 
	endif else begin
		for i = 0, count-1 do tree[i] = self -> ResolveURI(tree[i])
	end

	return, tree
end


;+
;   Filter by a specific version
;
; :Private:
;-
function MrURI::FilterTime, fileMatch, files, tstart, tend, $
COUNT=count, $
CLOSEST=closest, $
RELAXED_TSTART=relaxed_tstart, $
TIMEORDER=time_order, $
TPATTERN=tpattern
	compile_opt idl2
	on_error, 2
	
	;Filter both start and end times?
	tf_closest = keyword_set(closest)
	tf_tstart  = n_elements(tstart) gt 0 && tstart ne ''
	tf_tend    = n_elements(tend)   gt 0 && tend   ne ''
	if n_elements(time_order) eq 0 then time_order = '%Y%M%d%H%m%S'
	if n_elements(tpattern)   eq 0 then tpattern   = '%Y-%M-%dT%H:%m:%S'
	
	;No need to filter by time if tstart or tend are not provided
	if ~tf_tstart && ~tf_tend then return, files
	
	;Input file and directory name
	srchFile = self -> Path_BaseName(fileMatch)
	srchDir  = self -> Path_DirName(fileMatch)
	
	filesFound = self -> Path_BaseName(files)
	dirsFound  = self -> Path_DirName(files)

;-------------------------------------------
; Filter by Time ///////////////////////////
;-------------------------------------------
	;
	; Does the file name include TStart and TEnd?
	;   - Assume TEnd takes the same form as TStart.
	;   - Assume TStart does not repeat tokens.
	;   - Filenames include TStart and TEnd if the first token of TStart is repeated.
	;
	; Times are put into TimeOrder and converted to integers. This allows
	; for easy comparison. 
	;

	;Extract tokens
	tokens = MrTokens_Extract(srchFile, COUNT=nTokens, POSITIONS=token_pos)
	
	;Is there an end time in the file name?
	;   - Look for a repeated token
	;   - Repeated token will be found at position TOKEN_POS[0]+2+IREPEAT
	iRepeat = strpos( strmid(srchFile, token_pos[0]+2), '%'+tokens[0] )
	tf_fend = iRepeat ne -1

	;Convert the start time of each file to an integer
	;  - MrTimeParser will take the last match (i.e. FEND)
	;  - If the file name has an end time, parse up to the first repeated token.
	if tf_fend $
		then MrTimeParser, filesFound, strmid(srchFile, 0, iRepeat+2+token_pos[0]), time_order, fstart $
		else MrTimeParser, filesFound, srchFile, time_order, fstart
	ifstart = long64(fstart)

	;Convert the end time to an integer
	if tf_fend then begin
		MrTimeParser, filesFound, strmid(srchFile, iRepeat+token_pos[0]+2), time_order, fend
		ifend = long64(fend)
	endif
		
	;Convert input times to integers
	if tf_tstart then begin
		MrTimeParser, tstart, tpattern, time_order, temp_start
		itstart = long64(temporary(temp_start))
	endif
	if tf_tend then begin
		MrTimeParser, tend, tpattern, time_order, temp_tend
		itend = long64(temporary(temp_tend))
	endif

;-------------------------------------------
; Filename Includes End Time ///////////////
;-------------------------------------------
	;
	; We decide which files to keep by first considering what happens when
	; we have all information: tstart, tend, fStart, and fEnd. In this
	; case, we want to choose any files that contain any portion of the
	; time interval [tstart, tend]. 
	;
	;                    |----Time Interval----|
	;   [--File Interval--]        ....       [--File Interval--]
	;
	; This means any interval such that
	;   ( (tstart >= fStart) & (tstart <  fEnd) )
	; OR
	;   ( (tend   >  fStart) & (tend   <= fEnd) )
	;
	; If we have less information, we simply remove the clause containing
	; the missing information.
	;
	if tf_fend then begin
		case 1 of
			(tf_tstart && tf_tend): ikeep = where( ( (itstart ge ifstart) and (itstart le ifend) ) or $
				                                   ( (itend   ge ifstart) and (itend   le ifend) ), count )
			tf_tstart:              ikeep = where( (itstart ge ifstart) and (itstart le ifend), count )
			tf_tend:                ikeep = where( (itend   ge ifstart) and (itend   le ifend), count )
		endcase

;-------------------------------------------
; Filename Does Not Include End Time ///////
;-------------------------------------------
	endif else begin
		case 1 of
			(tf_tstart && tf_tend): ikeep = where( (itstart ge ifstart) or (itend ge ifstart), count )
			tf_tstart:              ikeep = where(itstart ge ifStart, count)
			tf_tend:                ikeep = where(itend   ge ifStart, count)
		end
	endelse
	
	;Select the subset of files
	if count gt 0 then begin
		dirsFound  = dirsFound[ikeep]
		filesFound = filesFound[ikeep]
		fstart     = fstart[ikeep]
		if tf_fend then fend = fend[ikeep]
	endif else begin
		dirsFound  = ''
		filesFound = ''
	endelse

;-------------------------------------------
; Closest Time /////////////////////////////
;-------------------------------------------
	;
	; We want to find the closes time to 'TStart'
	;   - If the file has both a start and end time, there is
	;     sufficient information to select the appropriate files.
	;     We do not need to check anything.
	;   - If only a start time exists in the file name, then the
	;     selection process above may be too liberal. Find the
	;     file that starts at or just before 'TStart'.
	;   - If 'TEnd' was also given, find the file that starts
	;     just before 'TEnd', and select all files between
	;     'TStart' and 'TEnd'. Otherwise, just pick the file
	;     associated with 'TStart'.
	;
	if tf_closest && ~tf_fend && count gt 0 then begin
		;
		; Find the file that starts closest in time to TSTART
		;

		;Find the largest start time <= TSTART
		istart = where(fstart le itstart, nstart)
		if nstart eq 0 then begin
			istart = 0
			nstart = 1
		endif
		void   = max(fstart[istart], imax)
		istart = istart[imax]
		
		;Find the smallest end time >= TEND
		if tf_tend then begin
			iend = where(fstart le itend, nend)
			void = max(fstart[iend], imax)
			iend = iend[imax]
		endif else begin
			iend = istart
			nend = 0
		endelse

		;Select the found files
		if nstart + nend gt 0 then begin
			if istart gt iend then message, 'TSTART must be before TEND.'
			dirsFound  = dirsFound[istart:iend]
			filesFound = filesFound[istart:iend]
			count      = iend - istart + 1
		endif else begin
			dirsFound  = ''
			filesFound = ''
			count      = 0
		endelse
	endif

	;Create the full file path
	;   - TODO: URLs use "/" as path separators whereas Windows uses "\".
	for i = 0, count - 1 do filesFound[i] = filepath(filesFound[i], ROOT_DIR=dirsFound[i])
	
	;Return
	return, filesFound
end


;+
;   Filter by a specific version
;
; :Private:
;-
function MrURI::FilterVersion, files, $
COUNT=count, $
NEWEST=newest, $
VERSION=version, $
VREGEX=vRegex
	compile_opt idl2
	on_error, 2
	
	;Get the newest version
	if n_elements(version) eq 0 then version = ''
	if n_elements(vRegex)  eq 0 then vRegex  = '([0-9]+)\.([0-9]+)\.([0-9]+)'
	newest = keyword_set(newest) || version eq ''
	
	;Separate the filename from the basename to focus on versions.
	nFiles   = n_elements(files)
	allFiles = file_basename(files)
	allDirs  = file_dirname(files)
	
;-------------------------------------------
; Find Uniq Files //////////////////////////
;-------------------------------------------
	;Replace the version number with the empty string
	vpos = stregex(allFiles, vRegex, LENGTH=vlen)
	if nFiles eq 1 then begin
		vpos = [vpos]
		vlen = [vlen]
	endif

	;Concatenate the parts before and after the version number
	files_noversion = strmid(allFiles, 0, transpose(vpos)) + $              ;Before version
	                  strmid(allFiles, transpose(vpos) + transpose(vlen))   ;After version

	;Find unique values
	iUniq = MrUniq(files_noversion, IARRAY=iArray, NUNIQ=count, /SORT)
	
	;Keep only the unique files
	dirsFound  = strarr(count)
	filesFound = strarr(count)

;-------------------------------------------
; Newest Version ///////////////////////////
;-------------------------------------------
	if newest then begin
		;Search through all unique files
		for i = 0, count - 1 do begin
			;Pick out the copies
			iCopies = where(iArray eq iUniq[i], nCopies)
			
			;Pick the first file as the newest
			newestFile = allFiles[iCopies[0]]
			newestDir  = allDirs[iCopies[0]]
			
			;Step through all copies
			for j = 1, nCopies - 1 do begin
				;Compare against remaining files
				if MrFile_VersionCompare(allFiles[iCopies[j]], newestFile, vRegex) eq 1 then begin
					newestFile = allFiles[iCopies[j]]
					newestDir  = allDirs[iCopies[j]]
				endif
			endfor
			
			;Replace unique files with newest file and directory
			dirsFound[i]  = newestDir
			filesFound[i] = newestFile
		endfor

;-------------------------------------------
; Specific Version /////////////////////////
;-------------------------------------------
	endif else if version ne '' then begin
		;Search through all unique files
		n = 0
		for i = 0, count - 1 do begin
			;Pick out the copies
			iCopies = where(iArray eq iUniq[i], nCopies)
			
			;Search for matches
			tf_match = MrFile_VersionCompare(allFiles[iCopies], version, vRegex) eq 0
			iMatch   = where(tf_match, nMatch)
			
			;Keep the first match
			if nMatch ne 0 then begin
				filesFound[n] = allFiles[iCopies[iMatch[0]]]
				dirsFound[n]  = allDirs[iCopies[iMatch[0]]]
				n++
			endif
		endfor
		count = n - 1

		;Trim results
		if count gt 0 then begin
			filesFound = filesFound[0:count]
			dirsFound  = dirsFound[0:count]
		endif else begin
			filesFound = ''
			dirsFound  = ''
		endelse
	endif

;-------------------------------------------
; Return Results ///////////////////////////
;-------------------------------------------
	;Pair directory and filename
	if count gt 0 then $
		for i = 0, count - 1 do filesFound[i] = filepath(filesFound[i], ROOT_DIR=dirsFound[i])
	if count eq 1 then filesFound = filesFound[0]
	

	return, filesFound
end


;+
;   Get object properties.
;
; :Keywords:
;       ALL:            in, optional, type=boolean
;                       If set, all links will be displayed. By default, only links
;                           starting with a number or a letter will be displayed.
;       DELAY:          out, optional, type=float
;                       Number of seconds to wait for a double click. If a second click
;                           is not detected before DELAY seconds, a single click event
;                           will be generated.
;-
pro MrURI::GetProperty, $
FRAGMENT=fragment, $
HOST=host, $
PASSWORD=password, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme, $
USERNAME=username, $
VERBOSE=verbose
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
stop
	;Set properties
	if arg_present(fragment) gt 0 then fragment = self.fragment
	if arg_present(host)     gt 0 then host     = self.host
	if arg_present(password) gt 0 then password = self.password
	if arg_present(path)     gt 0 then path     = self.path
	if arg_present(port)     gt 0 then port     = self.port
	if arg_present(query)    gt 0 then query    = self.query
	if arg_present(scheme)   gt 0 then scheme   = self.scheme
	if arg_present(username) gt 0 then username = self.username
	if arg_present(verbose)  gt 0 then verbose  = self.verbose
end


;+
;   Set object properties.
;
; :Keywords:
;       ALL:            in, optional, type=boolean
;                       If set, all links will be displayed. By default, only links
;                           starting with a number or a letter will be displayed.
;       DELAY:          in, optional, type=float
;                       Number of seconds to wait for a double click. If a second click
;                           is not detected before DELAY seconds, a single click event
;                           will be generated.
;-
pro MrURI::SetProperty, $
VERBOSE=verbose
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif

	;Set properties
	if n_elements(verbose) gt 0 then self.verbose = keyword_set(verbose)
end


;+
;   Cleanup after the object is destroyed. This will destroy the widget, if it exists.
;-
pro MrURI::Cleanup
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Free pointers
	ptr_free, self.field_values

	;Clean-up superclasses
	self -> IDL_Object::Cleanup
end


;+
;   The initialization method.
;
; :Params:
;       URL:            in, optional, type=string, default='http://www.google.com'
;                       Web address from which to download data.
;
; :Keywords:
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all links will be displayed. By default, only links
;                           starting with a number or a letter will be displayed.
;       DELAY:          in, optional, type=float, default=0.2
;                       Number of seconds to wait for a double click. If a second click
;                           is not detected before DELAY seconds, a single click event
;                           will be generated.
;       DIRECTORY:      in, optional, type=string
;                       Directory in which to save downloaded files.
;       GROUP_LEADER:   in, optional, type=long
;                       Widget ID of the widget that will serve as the group leader.
;
; :Returns:
;       If successful, a valid MrURI object will be returned.
;-
function MrURI::init, uri, $
VERBOSE=verbose
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif

	;Defaults
	self.verbose = keyword_set(verbose)
	if n_elements(uri) gt 0 then self -> CD, uri
	
	;Allocate heap
	self.field_values = ptr_new(/ALLOCATE_HEAP)

	return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:              out, optional, type=structure
;                           Class definition structure.
;
; :Fields:
;       ALL:            Display all links?
;       DELAY:          Max time to wait for a double click.
;       LOCAL_LIST:     List of files in the local directory to which files are saved.
;       LOCAL_PWD:      Present working directory upon initialization.
;       NCLICKS:        Number of clicks: single or double.
;       TLB:            Widget ID of the top-level base.
;       WEB_LIST:       List of links at the current web path.
;       WEBGET:         Object for navigating and downloading from the web.
;-
pro MrURI__Define, class
	compile_opt idl2

	class = { MrURI, $
	          inherits IDL_Object, $
	          fragment:     '', $
	          field_values: ptr_new(), $
	          host:         '', $
	          password:     '', $
	          path:         '', $
	          port:         '', $
	          query:        '', $
	          scheme:       '', $
	          username:     '', $
	          verbose:      0B $
	        }
end
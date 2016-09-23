; docformat = 'rst'
;
; NAME:
;       MrWebURI__Define
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
;       2016-06-24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrWebURI::_OverloadPrint
	compile_opt idl2
	on_error, 2
	
	;Get info from the superclass
	uri_print = self -> MrURI::_OverloadPrint()
	
	;Create strings
	offline     = string('  Offline',     '=', self.offline,     FORMAT='(a-26, a-2, i1)')
	no_download = string('  No_Download', '=', self.no_download, FORMAT='(a-26, a-2, i1)')
	local_root  = string('  Local_Root',  '=', self.local_root,  FORMAT='(a-26, a-2, a0)')
	mirror_root = string('  Mirror_Root', '=', self.mirror_root, FORMAT='(a-26, a-2, a0)')
	remote_root = string('  Remote_Root', '=', self.remote_root, FORMAT='(a-26, a-2, a0)')

	;Output array
	outStr = [ [ uri_print   ], $
	           [ offline     ], $
	           [ no_download ], $
	           [ local_root  ], $
	           [ mirror_root ], $
	           [ remote_root ] $
	         ]
	
	;Sort alphabetically
	outStr = outStr[0,sort(outStr)]

	;Print the array
	return, outStr
end

;+
;   Able to recursively download files from an HTTP address, given a URI or URI pattern.
;
;   NOTES:
;       Errors:     If an error occurs such that a file is only partially downloaded,
;                   the partial download will be deleted. (Does not apply for /WGET)
;
; :Examples:
;   See the examples in the FindURL method. Any URL that FindURL can find/parse can
;   be used for download.
;
;   Download 4 files from the Van Allen Probes satellite mission::
;       host = 'http://www.rbsp-ect.lanl.gov/'
;       url  = 'data_pub/rbsp%((a|b)%)/MagEphem/def/2013/rbsp%((a|b)%)_def_MagEphem_TS04D_2013010%([1-2]%)_v*.h5'
;       oWGet = MrWebURI(host)
;       oWGet -> Download, url
;
;   Download the same 4 files WGet (requires MrWGet)::
;       oWGet -> Download, url, /WGET
;
; :Params:
;       URL:            in, required, type=string
;                       URL or URL pattern to be found.
;       OUTPUTDIR:      in, optional, type=string/strarr, default=pwd
;                       Directory name in which to download data. If an array, it must
;                           contain the same number of elements as `URL`
;
; :Keywords:
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, files that already exist locally will not be downloaded.
;       NO_PROGRESSBAR: in, optional, type=boolean, default-0
;                       If set, no progress bar will be created.
;       QUIET:          in, optional, type=boolean, defualt=0
;                       If set, informational status messages will not be printed.
;       TIME_STAMPING:  in, optional, type=boolean, default=0
;                       If set, files that already exist locally will not be downloaded
;                           again if URL is the same age or older. This keyword is
;                           ignored if `NO_CLOBBER` is set. This may fail, depending on
;                           how the time stamp is formatted by the host.
;       WGET:           in, optional, type=boolean, default=0
;                       If set, wGet will be used to download the files instead of
;                           IDLnetURL. See MrWGet.pro for details.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrWGet.pro is also accepted via keyword
;                           inheritance. `WGET` must be set. By default
;                           NO_HOST_DIRECTORIES, NO_PARENT, NO_DIRECTORIES, SHOW_COMMAND,
;                           and DIRECTORY_PREFIX are set.
;-
function MrWebURI::Download, uri, outputDir, $
NO_CLOBBER=no_clobber, $
NO_PROGRESSBAR=no_progressBar, $
QUIET=quiet, $
TIME_STAMPING=time_stamping, $
WGET=wGet, $
_REF_EXTRA=extra
	compile_opt idl2

;-----------------------------------------------------
; Error Handling \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(progressBar) then obj_destroy, progressBar
		if n_elements(callback) gt 0 then self -> SetProperty, CALLBACK_FUNCTION=callback
		
		;Call the error handler
		self -> Error_Handler
		
		;Change back to the original location
		self -> CD, thisURI
	
		;Delete the partial file?
		if n_elements(fileOut) gt 0 && file_test(fileOut) then begin
			if quiet eq 0 then message, 'Deleting partial file "' + fileOut + '".', /INFORMATIONAL
			file_delete, fileOut
		endif
	
		count = 0
		return, ''
	endif

;-----------------------------------------------------
; Check Inputs\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the current URI
	thisURI = self -> GetURI()
	
	;Defaults
	quiet         = keyword_set(quiet)
	makeBar       = ~keyword_set(no_progressBar)
	wget          = keyword_set(wget)
	time_stamping = keyword_set(time_stamping)
	no_clobber    = keyword_set(no_clobber)
	
	;Output directories
	nURIs = n_elements(uri)
	nDirs  = n_elements(outputDir)
	if nDirs eq 0 then outputDir = file_basename(self -> uri2dir(uri))
	if nDirs ne 1 && nDirs ne nURIs $
		then message, 'OUTPUTDIR must be scalar or have the same number of elements as FILES.'

;-----------------------------------------------------
; WGet? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if wget then begin
		;If anything in EXTRA is given, it will override the default choices
		;written explicity in the command.
		MrWGet, uri, DIRECTORY_PREFIX=self.directory, /SHOW_COMMAND, $
		             /NO_PARENT, /NO_DIRECTORIES, /NO_HOST_DIRECTORIES, $
		             TIME_STAMPING=time_stamping, _STRICT_EXTRA=extra
		return, uri
	endif

;-----------------------------------------------------
; IDLnetURL? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the callback function
	self -> GetProperty, CALLBACK_FUNCTION=callback

	;Parse the URIs to get the file names
	self -> ParseURI, uri, SCHEME=scheme, HOST=host, PATH=path
	filename = self -> Path_BaseName(path)

	;Step through each file
	for i = 0, nURIs-1 do begin
		;Create the output filename
		fileOut = filepath(filename[i], ROOT_DIR=outputDir[i])
		
		;Make sure the out put directory exists
		;   - If not, CURL will throw an error (code 23)
		if ~file_test(outputDir[i], /DIRECTORY) then file_mkdir, outputDir[i]

	;-----------------------------------------------------
	; Clobber \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if no_clobber then begin
			if file_test(fileOut) then begin
				if quiet eq 0 then $
					print, 'File exists. Skipping "' + fname_check + '".'
				continue
			endif
		endif
	
	;-----------------------------------------------------
	; Time Stamping \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Check if local file is newer. 
		;   If NO_CLOBBER is set, matching files will have already been skipped.
		if time_stamping then begin
			;Check if the file already exists
			fname_check = file_info(fileOut)
			if fname_check.exists then begin
				;Convert file's modified time to ISO-1806
				utc_string = systime(0, fname_check.mtime, /UTC)
				patternIn  = '%w %c %d %H:%m:%S %Y'
				patternOut = '%Y-%M-%dT%H:%m:%SZ'
				MrTimeParser, utc_string, patternIn, patternOut, mTime
			
				;Get its time stamp of the file being downloaded and convert it to
				;system time
				rHeader = self -> ParseResponseHeader(TIME_STAMP=time_stamp)
			
				;Compare the times
				if mTime ge time_stamp then begin
					if quiet eq 0 then message, 'Local file newer. Skipping "' + filename + '".', /INFORMATIONAL
					continue
				endif
			endif
		endif

	;-----------------------------------------------------
	; Progress Bar \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if makeBar then begin
			;theText = string(FORMAT='(%"File %i of %i.")', i+1, nFiles)    ;Another choice for the text string.
			progressBar = obj_new('cgProgressBar', TEXT=fileOut, TITLE='Downloading...', $
			                      /CANCEL, /START)
		
			;Create and set the callback structure
			callbackData = { bar:          progressBar, $
			                 last_update:  0.0, $
			                 last_size:    0L, $
			                 oNet:         self, $
			                 percent:      0B, $
			                 total_size:   0LL $
			               }
			self -> SetProperty, CALLBACK_DATA=callbackData, CALLBACK_FUNCTION='MrWebURI_Callback_Download'
		endif else begin
			self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'
			if quiet eq 0 then begin
				theText = string(FORMAT='(%"%i of %i")', i+1, nFiles)
				print, 'Downloading ' + theText + ':  ' + uri[i]
			endif
		endelse
	;-----------------------------------------------------
	; Download \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; Using the ::Get method with the URL keyword ignores
		; other URL_* properties, meaning the username and password
		; get lost. Instead, CD to the location of the file, then
		; call ::Get.
		;
		
		;CD to location of file
		self -> CD, uri[i]
		
		;Download file
		fileOut = self -> IDLnetURL::Get(FILENAME=fileOut)

	;-----------------------------------------------------
	; Cancel? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if makeBar then begin
			cancel = progressBar -> CheckCancel()
			obj_destroy, progressBar
	
			;Break out of the loop if the user cancelled.
			if cancel then break
		endif
	endfor

	;Reset the callback function
	self -> SetProperty, CALLBACK_FUNCTION=callback
	self -> CD, thisURI
	
	;Return file names
	return, fileOut
end


;+
;   Handle errors. Translate CCurlException errors into something useful.
;-
pro MrWebURI::Error_Handler
	compile_opt idl2

	;Reset the callback function
	self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'

	;Get the error code
	if (!error_state.name eq 'IDL_M_URL_ERRS2') || strpos(!error_state.msg, 'IDLnetURL') ne -1 then begin
		;Parse the response header
		self -> GetProperty, RESPONSE_HEADER=rh
		header = self -> ParseResponseHeader(rh)
		
		;Handle cases
		case header.code of
			  2:  MrPrintF, 'LogErr', 'Problem internal to Curl. Might require IDL restart.'
			 18:  MrPrintF, 'LogErr', 'Partial file transferred.'
			 42:  message, 'Download Canceled.', /INFORMATIONAL
			301:  MrPrintF, 'LogErr', 'Linked Moved. Try adding trailing "/".'
			400:  MrPrintF, 'LogErr', 'Bad Request. Check URL.'
			401:  MrPrintF, 'LogErr', 'Authorization required. Provide username and password.'
			404:  MrPrintF, 'LogErr', 'URL not found.'
			else: MrPrintF, 'LogErr', header.code, header.status, FORMAT='(%"Code: %i; Status: %s")'
		endcase
	endif else begin
		MrPrintF, 'LogErr'
	endelse
end


;+
;   Fill the drop list with the contents of the selected directory.
;
; :Example:
;   Get files from the UNH MMS data mirror::
;       IDL> oWeb = MrWebData()
;       IDL> files = oWeb -> Get('http://mmsdata.sr.unh.edu/mms1/edi/fast/l1a/amb/2015/08/*.cdf')
;-
function MrWebURI::Get, uri, $
COUNT=count, $
CLOSEST=closest, $
NEWEST=newest, $
TSTART=tstart, $
TEND=tend, $
TIMEORDER=time_order, $
TPATTERN=tpattern
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(oFile) gt 0 then obj_destroy, oFile
		MrPrintF, 'LogErr'
		return, ''
	endif

	;
	; The ::Get methods accepts MrTokens, but the ::CD method does not.
	; As such, the internal URI property is not necessarily the URI that
	; the user wants to find.
	;

;-----------------------------------------------------
; Search for Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Search locally
	if self.offline then begin
		;Create a local URI object
		;   - Not enough to simply use ::uri2dir because MrTokens may be present.
		oFile = MrFileURI('file://' + self.local_root)
		
		;Convert the URI to a directory
		file_uri = self -> uri2dir(uri)
		
		;Search for files
		files = oFile -> Search(file_uri)
		
		;Destroy the object
		obj_destroy, oFile
	
	;Search remotely
	endif else begin
		files = self -> Search(uri)
	endelse

;-----------------------------------------------------
; Filter Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time filter
	files = self -> FilterTime(uri, files, tstart, tend, $
	                           COUNT     = count, $
	                           CLOSEST   = closest, $
	                           TIMEORDER = time_order, $
	                           TPATTERN  = tpattern)
	if count eq 0 then message, 'No files in time range.'
	
	;Version filter
	files = self -> FilterVersion(files, $
	                              COUNT=count, $
	                              NEWEST=newest)
	if count eq 0 then message, 'No files with requested version.'

;-----------------------------------------------------
; Local Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory
	out_files = strarr(count)

	;Simulate what the local version would look like
	local_files = self -> uri2dir(files)
	
	;Check if files exist locally
	tf_local = file_test(local_files)
	
	;Which files do we need to download?
	iLocal = where(tf_local, nLocal, COMPLEMENT=iDownload, NCOMPLEMENT=nDownload)
	if nLocal gt 0 then out_files[iLocal] = local_files[iLocal]

;-----------------------------------------------------
; Remote Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Do not download
	if self.no_download then begin
		out_files = nLocal eq 0 ? '' : out_files[iLocal]
		count     = nLocal
		
	;Download
	endif else if nDownload gt 0 then begin
		out_files[iDownload] = self -> Download(files[iDownload], file_dirname(local_files[iDownload]))
	endif

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	if count eq 1 then out_files = out_files[0]
	return, out_files
end


;+
;   Get the directory listings.
;
; :Private:
;-
function MrWebURI::GetDirList, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Assume no matches
	count = 0

	;Get URL properties. Turn off the callback function
	self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'

	;Get Directories
	case self.scheme of
		'http':  dirList = self -> GetHttpDirList()
		'https': dirList = self -> GetHttpDirList()
		'ftp':   dirList = self -> GetFtpDirList(/SHORT)
		'ftps':  dirList = self -> GetFtpDirList(/SHORT)
		else: message, 'Scheme not recognized: "' + scheme + '".'
	endcase
	
	count = n_elements(dirList)
	return, dirList
end


;+
;   Treats all links in an HTTP address as directories and returns the address of
;   each link.
;
; :Private:
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of links found.
;       ERROR:          out, optional, type=integer
;                       Variable to recieve the error code. 0 indicates no error. If
;                           present, the error message will be supressed.
;
; :Returns:
;       LINKS:          An array containing the destination of each link on the page.
;                           Links are assumed to be in <a href="theLink"></a> tags.
;-
function MrWebURI::GetHttpDirList, $
COUNT=nLinks
	compile_opt idl2
	on_error, 2

	;Get the URL
	if strpos(self.scheme, 'http') eq -1 then $
		message, 'Scheme "' + self.scheme + '" is not {http | https}. Cannot get HTTPDirList'

	;HTTP directory listing (search for all links)
	strOut = self -> IDLnetURL::Get(/STRING_ARRAY)
	nLines = n_elements(strOut)

	;Step through each line looking for links. There could be more than one per line.
	links = strarr(nLines*10L)
	nLinks = 0
	for i = 0, nLines-1 do begin
		void = strsplit(strOut[i], '<a', /REGEX, /EXTRACT, COUNT=count)
		if strmid(strOut[i], 0, 2) ne '<a' then count -= 1
		if count le 0 then continue
	
		;Extract the links
		tempLinks = stregex(strOut[i], strjoin(replicate('<a href="([^"]*)">.*', count)), $
		                    /SUBEXP, /EXTRACT)

		links[nLinks:nLinks+count-1] = reform(tempLinks[1,*])
		nLinks += count
	endfor

	;Trim results
	links = links[0:nLinks-1]
	return, links
end


;+
;   Get the response header without downloading all of the information with ::Get.
;
; :Params:
;       URI:                in, optional, type=string, default=current URI
;                           URL for which the response header is desired.
;
; :Returns:
;       RESPONSE_HEADER:    The response header of the `URL`.
;-
function MrWebURI::GetResponseHeader, uri
	compile_opt idl2
	on_error, 2

	;Change the callback function
	self -> GetProperty, CALLBACK_FUNCTION=callback_in
	self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback_ResponseHeader'

	;Get the URL, but short-circuit as soon as the header is downloaded
	;   - Use a catch because cancelling the callback function will trigger an error
	;   - Do not set a default URL so that the username and password will be sent
	catch, the_error
	if the_error eq 0 $
		then output = self -> IDLnetURL::Get(URL=uri, /BUFFER) $
		else catch, /CANCEL

	;Get the response header
	self -> GetProperty, RESPONSE_HEADER=response_header

	;Reset the callback function
	self -> SetProperty, CALLBACK_FUNCTION=callback_in

	return, response_header
end


;+
;   Set the URI.
;
; :Params:
;       URL:            in, required, type=string
;                       The fully resolved URI to be made the current uri.
;-
pro MrWebURI::GetProperty, $
DROPBOX_ROOT=dropbox_root, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
REMOTE_ROOT=remote_root, $
FRAGMENT=fragment, $
HOST=host, $
PATH=path, $
PORT=port, $
QUERY=query, $
SCHEME=scheme, $
VERBOSE=verbose, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Set properties
	if arg_present(dropbox_root) gt 0 then dropbox_root = self.dropbox_root
	if arg_present(local_root)   gt 0 then local_root   = self.local_root
	if arg_present(mirror_root)  gt 0 then mirror_root  = self.mirror_root
	if arg_present(remote_root)  gt 0 then remote_root  = self.remote_root

	;MrURI Properties
	if arg_present(fragment) gt 0 then fragment = self.fragment
	if arg_present(host)     gt 0 then host     = self.host
	if arg_present(path)     gt 0 then path     = self.path
	if arg_present(port)     gt 0 then port     = self.port
	if arg_present(query)    gt 0 then query    = self.query
	if arg_present(scheme)   gt 0 then scheme   = self.scheme
	if arg_present(verbose)  gt 0 then verbose  = self.verbose

	;NetURL properties
	if n_elements(extra) gt 0 then self -> IDLnetURL::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Recursively search the link structure.
;
; :Params:
;       URL:            in, required, type=string
;                       URL at which to begin crawling.
;
; :Keywords:
;       NLEVELS:        in, optional, type=integer, default=3
;                       Maximum number of levels to recursively search.
;       STEP:           in, private, type=integer
;                       The current recursive step.
;       URLCRAWL:       out, optional, type=strarr
;                       URLs encountered during the crawl.
;
; :Returns:
;       TYPE:           The Content-Type for each link encountered.
;-
function MrWebURI::HttpCrawl, URI, $
NLEVELS=nLevels, $
STEP=step, $
URLCRAWL=urlCrawl
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		step -= 1
		return, ''
	endif

	;Defaults
	parentURI = (n_elements(URI) gt 0) ? URI : self -> GetURI()
	if n_elements(nLevels)   eq 0 then nLevels   = 3
	if n_elements(pathCrawl) eq 0 then pathCrawl = parentURI
	if n_elements(step)      eq 0 then step      = 0 else step += 1

	;Turn off the callback function
	if step eq 0 then begin
		self -> GetProperty, CALLBACK_FUNCTION=callback_fn
		self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'
	endif

	;Get the current URI
	currentURI = self -> GetURI()

	;Change to the given drictory and get its content type
	if parentURI ne currentURI then self -> CD, parentURI
	void = self -> ParseResponseHeader(CONTENT_TYPE=parentType)

	;Store the URI in the crawler list.
	if n_elements(uriCrawl) eq 0 $
		then uriCrawl = parentURI $
		else uriCrawl = [uriCrawl, parentURI]

	;Return if
	; - The URI is not a webpage
	; - We reached the recursion limit
	if (parentType ne 'html') || (step ge nLevels) then begin
		step -= 1
		self -> SetURI, currentURI
		return, parentType
	endif

	;Get the list of directories for the requested uri
	self -> LS, '^[^?].*', /REGEX, COUNT=nChildren, OUTPUT=children
	if nChildren eq 0 then begin
		step -= 1
		self -> SetURI, currentURI
		return, parentType
	endif

	;Resolve the links
	childURIs = self -> ResolveURI(children)

	;Find external links
	self -> ParseURI, childURIs, HOST=host
	tf_external = host ne self.host

	;Has the page been crawled before?
	tf_crawled = IsMember(uriCrawl, childURIs)

	;Step through each link and recursively get other links.
	; - Do not follow external links
	; - Do not crawl the same page twice
	tf_result = 0
	for i = 0, nChildren - 1 do begin
		if tf_external[i] eq 1 then continue
		if tf_crawled[i]  eq 1 then continue

		;Follow the link
		childTypes = self -> HttpCrawl(childURIs[i], NLEVELS=nLevels, STEP=step, $
		                               URICRAWL=uriCrawl)
		if tf_result eq 0 then begin
			result = [parentType, childTypes]
			tf_result = 1
		endif else result = [result, childTypes]
	endfor

	;Are all links external and/or pre-crawled
	if n_elements(result) eq 0 then begin
		step -= 1
		self -> SetURI, currentURI
		return, parentURI
	endif

	;Move back up the tree
	step -= 1
	self -> SetURI, currentURI

	;Turn the callback function back on
	if step eq 0 then self -> SetProperty, CALLBACK_FUNCTION=callback_fn
	return, result
end


;+
;   Log-In to a website.
;
; :Private:
;
; :Params:
;       USERNAME:       in, optional, type=string
;                       Log-in username. If first log-in try is unsuccessful, a dialog
;                           box will appear asking for the username.
;       PASSWORD:       in, optional, type=string
;                       Log-in password. If first log-in try is unsuccessful, a dialog
;                           box will appear asking for the password.
;-
pro MrWebURI::Login, username, password, $
GROUP_LEADER=group_leader
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(oLogin) gt 0 then obj_destroy, oLogin
		self -> Error_Handler
		return
	endif

	;Parse the URL
	status = 0
	self -> IDLnetURL::SetProperty, URL_USERNAME = username, $
	                                URL_PASSWORD = password

	;
	;The callback routine aborts, so the response code property
	;will not be the response code of the header. Parse the code
	;from the header
	;
	
	;Keep trying until successful or until the user gives up.
	;   - '401 Authorization Requied'
	;   - '401 Unauthorized'
	cancel   = 0
	nTries   = 0
	maxTries = 3
	tf_retry = 1

	;Log-In
	while ~cancel && tf_retry && nTries le maxTries do begin
		;Login
		oLogin = obj_new('MrLogin', GROUP_LEADER=group_leader)
		oLogin -> GetProperty, USERNAME=username, PASSWORD=password, CANCEL=cancel
		obj_destroy, oLogin
	
		;Check what happened
		if ~cancel then begin
			;Input log-in credentials and check if they were accepted
			self -> IDLnetURL::SetProperty, URL_USERNAME=username, URL_PASSWORD=password
			header   = self -> GetResponseHeader()
			tf_retry = stregex(header, '401 (Un)?Author', /FOLD_CASE, /BOOLEAN)

			;Indicate error
			if tf_retry then begin
				if nTries lt maxTries-1 $
					then MrPrintF, 'LogWarn', 'Incorrect username or password. Try again.' $
					else message, 'Incorrect username or password.'
			endif else begin
				self.username = username
				self.password = password
				self -> IDLnetURL::SetProperty, URL_USERNAME=username, URL_PASSWORD=password
			endelse
		endif
		
		nTries += 1
	endwhile
end


;+
;   Log-In to the CSA
;
; :Params:
;       USERNAME:           in, required, type=string
;                           Username on the SDC team website.
;       PASSWORD:           in, required, type=string
;                           Password for the SDC team website.
;-
pro MrWebURI::LogIn2, username, password, $
CANCEL=tf_cancel, $
GROUP_LEADER=group_leader, $
NO_GUI=no_gui
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if n_elements(oLogIn) gt 0 then obj_destroy, oLogIn
		MrPrintF, 'LogErr'
		return
	endif
	
	;Default to creating a gui
	tf_gui = ~keyword_set(no_gui)
	
	;GUI login
	if tf_gui then begin
		;LogIn widget
		oLogIn = MrLogIn( GROUP_LEADER = group_leader, $
		                  USERNAME     = username, $
		                  PASSWORD     = password )
		
		;Cancelled?
		tf_cancel = oLogIn.cancel
		if tf_cancel then return
		
		;Get username and password
		oLogIn -> GetProperty, USERNAME = username, $
		                       PASSWORD = password
		
		;Destroy the login object
		obj_destroy, oLogIn
	
	;Prompt login
	endif else begin
		username = ''
		password = ''
		
		;Prompt for username
		read, username, PROMPT='username: '
		if username eq '' then return
		
		;Hide password
		if MrCmpVersion('8.0.0') le 0 then begin
			;Print prompt, without new line.
			;   - The "$" character will prevents newline implied by final tick (').
			print, format='($, A)', 'password: '

			;Gather characters until the user hits <return>.
			;    8B = Backspace
			;   10B = Linefeed
			;   13B = Carriage Return
			while (1) do begin
				ch = get_kbrd(/ESCAPE) ;read a character from the keyboard
				b  = byte(ch)
				if (b eq 13B or b eq 10B) then begin ;return
					print, '' ;Print a new line to clear the prompt
					break     ;Exit the loop
				endif
				
				;Remove a character if backspace
				if b eq 8B then begin
					password = strmid(password, 0, strlen(password)-1)
				;Otherwise, add a character
				endif else begin
					password += ch ;append character to the password
					print, format='($, A)', '*'
				endelse
			endwhile
		
		;Visible password
		endif else begin
			;Prompt for visible password
			read, password, prompt='password: '
		endelse
	endelse
	
	;Set username
	if username ne '' && password ne '' then begin
		self -> IDLnetURL::SetProperty, URL_PASSWORD = password, $
		                                URL_USERNAME = username
	endif
end


;+
;   Log-Out of a website.
;-
pro MrWebURI::LogOut
	compile_opt idl2
	on_error, 2

	;Parse the URL
	self.username = ''
	self.password = ''
	self -> IDLnetURL::SetProperty, URL_USERNAME='', $
	                                URL_PASSWORD=''
	
	;Close connections
	self -> CloseConnections
end


;+
;   Get response header without downloading all of the data.
;
; :Keywords:
;       CODE:           out, optional, type=integer
;                       Error respose code. 200 indicates no error.
;       COUNT:          out, optional, type=integer
;                       Number of links found.
;       ERROR_MSG:      out, optional, type=string
;                       Error response message. "OK" indicates no error.
;       CONTENT_TYPE:   out, optional, type=string
;                       MIME type of the content.
;       SHELL_ERROR:    out, optional, type=string
;                       Error returned from the shell when spawingin wGet.
;       TIME_STAMP:     out, optional, type=string
;                       Time stamp given to the URL. Returned in the ISO-1806 format
;                           '2006-03-23T23:13:16Z' when the time stamp is recognized.
;
; :Returns:
;       RESPONSE:       Response header
;-
function MrWebURI::ParseResponseHeader, rh, $
TIME_STAMP=time_stamp
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Break by line
	temp = strsplit(rh, string([10B,13B]), /EXTRACT, COUNT=nLines)
	if nLines eq 0 then begin
		self -> GetProperty, RESPONSE_CODE=rc
		header = create_struct('code', rc, 'status', 'Failure.')
		return, header
	endif
	
	;Parse the header
	;   - Status line first
	;   - Header information next
	str   = stregex(temp[0], 'HTTP/[1-9.]+ ([0-9]{3}) ([^' + string(10B) + string(13B) + ']+)', /SUBEXP, /EXTRACT)
	parts = stregex(temp[1:*], '([a-zA-Z1-9-]+): (.*)', /SUBEXP, /EXTRACT)
	
	;Convert to tag-value pairs
	;   - Convert all tag names to valid IDL tags
	tags   = idl_validname( reform(parts[1,*]), /CONVERT_ALL, /CONVERT_SPACES )
	values = reform(parts[2,*])
	
	;Search for duplicate tags
	;   - Order of header fields does not matter, but keep them the same anyway
	;   - Duplicate header fileds should be treated comma separated list. Order is important!
	;   - https://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2
	header = create_struct( 'code', fix(str[1]), 'status', str[2] )
	nKeep = n_elements(tags)
	while nkeep gt 0 do begin
		;Find duplicate entries and combine their values
		iValues = where( tags eq tags[0], nValues, COMPLEMENT=iKeep, NCOMPLEMENT=nKeep )
		header  = create_struct( header, tags[0], strjoin(values[iValues], ', ') )
		
		;Throw away old tag-value pairs
		if nKeep gt 0 then begin
			tags = tags[iKeep]
			values = values[iKeep]
		endif
	endwhile

;-----------------------------------------------------
; Time Stamp \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if arg_present(time_stamp) then begin
		str     = stregex(response, 'Last-Modified: (.*)', /SUBEXP, /EXTRACT)
		ts_temp = str[1]
	
		;Was the time stamp found? It can have 3 formats
		if ~MrStruct_HasTag(header, 'last_modified') then begin
			patternOut = '%Y-%M-%dT%H:%m:%S %z'             ;1994-05-06T08:49:37 GMT
			pattern = ['%w, %d %c %Y %H:%m:%S %z', $        ;Sun, 06 Nov 1994 08:49:37 GMT     (preferred)
			           '%w, %d %c %Y %H:%m:%S %o', $        ;Sun, 06 May 1994 08:49:37 +0000
			           '%W, %d-%c-%y %H:%m:%S %z', $        ;Sunday, 06-Nov-94 08:49:37 GMT
			           '%w %c [0-9]?[0-9] %H:%m:%S %Y']     ;Sun Nov  6 08:49:37 1994
		
			;Convert the time and check for a match
			tf_match = 0
			count = 0
			while tf_match eq 0 && count lt n_elements(pattern)-1 do begin
				MrTimeParser, header.last_modified, pattern[count], patternOut, time_stamp, ISMATCH=tf_match
				count++
			endwhile
		
			;No matches?
			if tf_match eq 0 then $
				message, 'Time Stamp format "' + header.last_modified + '" not recognized.', /INFORMATIONAL
		endif
	
		if n_elements(time_stamp) eq 0 then time_stamp = ''
	endif

	return, header
end


;+
;   Set the URI.
;
; :Params:
;       URL:            in, required, type=string
;                       The fully resolved URI to be made the current uri.
;-
pro MrWebURI::SetURI, uri
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Parse URI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Parse the URL
	;   - Take scheme from object property -- relative path could be given.
	;   - Implies the scheme property must be correct upon entry
	self -> ParseURI, uri, $
	                  FRAGMENT  = fragment, $
	                  HOST      = host, $
	                  PASSWORD  = password, $
	                  PATH      = path, $
	                  PORT      = port, $
	                  QUERY     = query, $
	                  SCHEME    = scheme, $
	                  USERNAME  = username
	
	;::ParseURI returns the path with a leading "/". We must remove them
	;to prevent them from compounding when IDLnetURL::SetProperty is called.
	pos = stregex(path, '^/+')
	if pos ne -1 then path = strmid(path, pos+1)

;-----------------------------------------------------
; Set the URI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Rules:
	;   - A fully resolved URI is expected as input
	;   - PORT cannot be a null string
	;   - Do not reset user information. Call to ::LogIn will verify login info.
;	if fragment eq '' then void = temporary(fragment)
;	if host     eq '' then void = temporary(host)
;	if path     eq '' then void = temporary(path)
;	if query    eq '' then void = temporary(query)
	if scheme   eq '' then message, 'The URI must have a scheme.'
	if password eq '' then void = temporary(password)
	if port     eq '' then void = temporary(port)
	if username eq '' then void = temporary(username)
	
	;Set for the URL object as well
	self -> IDLnetURL::SetProperty, URL_HOSTNAME = host, $
	                                URL_PATH     = path, $
	                                URL_QUERY    = query, $
	                                URL_USERNAME = username, $
	                                URL_PORT     = port, $
	                                URL_SCHEME   = scheme, $
	                                URL_PASSWORD = password

;-----------------------------------------------------
; Check For Success \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check if everything was successful
	rh     = self -> GetResponseHeader()
	header = self -> ParseResponseHeader(rh)
	reset_info = 0B

	;Handle various cases
	if header.code ne 200 then begin
		case header.code of
			;Re-route moved URIs
			301: begin
				MrPrintF, 'LogWarn', 'URI Moved'
				MrPrintF, 'LogWarn', '   From: ' + uri
				MrPrintF, 'LogWarn', '   To:   ' + header.location
			endcase
			
			;URL Found (redirection)
			302: begin
				MrPrintF, 'LogWarn', 'URL Found'
				MrPrintF, 'LogWarn', '   Location: ' + header.location
				self -> CD, header.location
				return
			endcase
			
			;Unauthorized
			401: begin
				MrPrintF, 'Log-In required.'
				self -> LogIn
			endcase
			
			;URI not found
			404: begin
				MrPrintF, 'LogErr', header.code, header.status, uri, FORMAT='(%"%i: %s; \"%s\"")'
				reset_info = 1B
			endcase
			
			;Use the error handler
			else: begin
				self -> Error_Handler
				reset_info = 1B
			endcase
		endcase
	endif

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Set properties
	if reset_info then begin
		self -> IDLnetURL::SetProperty, URL_HOSTNAME = self.host, $
		                                URL_PATH     = self.path, $
		                                URL_QUERY    = self.query, $
;		                                URL_USERNAME = username, $
;		                                URL_PORT     = port, $
		                                URL_SCHEME   = self.scheme
;		                                URL_PASSWORD = self.password
	endif else begin
		self.fragment = fragment
		self.host     = host
		self.path     = path
		self.query    = query
		self.scheme   = scheme
		if n_elements(username) gt 0 then self.username  = username
		if n_elements(password) gt 0 then self.password  = password
		if n_elements(port)     gt 0 then self.port      = port
	endelse
end


;+
;   Set the URI.
;
; :Params:
;       URL:            in, required, type=string
;                       The fully resolved URI to be made the current uri.
;
; :Keywords:
;       CALLBACK_FUNCTION:  in, optional, type=string
;                           The name of a callback function used when transferring data.
;       CALLBACK_DATA:      in, optional, type=any
;                           Data to be made available to the `CALLBACK_FUNCTION`.
;       DROPBOX_ROOT:       in, optional, type=string
;                           Local folder in which files are temporarily placed before
;                               being moved to into `LOCAL_ROOT`.
;       LOCAL_ROOT:         in, optional, type=string
;                           Local directory root where downloaded files are to be saved.
;                               The underlying directory structure should mimic the remote
;                               site from which data was obtained.
;       MIRROR_ROOT:        in, optional, type=string
;                           Remote root directory of a data mirror site.
;       OFFLINE:            in, optional, type=boolean
;                           If set, the object will function in offline mode (search for
;                               files locally). Automatically sets `NO_DOWNLOAD` to the same value.
;       NO_DOWNLOAD:        in, optional, type=boolean
;                           If set, use only those files that are saved locally. Files are
;                               still searched for on the remote server. Remote files are
;                               normally downloaded if they are more recent and/or have a
;                               higher version number than local files.
;       REMOTE_ROOT:        in, optional, type=string
;                           Remote root directory where data is saved.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the superclass MrURI::SetProperty
;                               method is also accepted via keyword inheritance.
;-
pro MrWebURI::SetProperty, $
CALLBACK_DATA=callback_data, $
CALLBACK_FUNCTION=callback_function, $
DROPBOX_ROOT=dropbox_root, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
NO_DOWNLOAD=no_download, $
OFFLINE=offline, $
REMOTE_ROOT=remote_root, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Set properties
	if n_elements(dropbox_root) gt 0 then self.dropbox_root = dropbox_root
	if n_elements(local_root)   gt 0 then self.local_root   = local_root
	if n_elements(mirror_root)  gt 0 then self.mirror_root  = mirror_root
	if n_elements(remote_root)  gt 0 then self.remote_root  = remote_root
	
	;Offline options
	;   - NO_DOWNLOAD depends on OFFLINE
	if n_elements(offline) gt 0 then begin
		self.offline = keyword_set(offline)
		no_download  = self.offline ? 1B : 0B
	endif
	if n_elements(no_download) gt 0 then self.no_download = keyword_set(no_download)

	;NetURL Properties
	self -> IDLnetURL::SetProperty, CALLBACK_DATA     = callback_data, $
	                                CALLBACK_FUNCTION = callback_function

	;Superclass properties
	if n_elements(extra) gt 0 then self -> MrURI::SetProperty, _STRICT_EXTRA=extra
end


;+
;   Convert a web URI to a directory structure.
;
; :Private:
;-
function MrWebURI::uri2dir, uri
	compile_opt strictarr
	on_error, 2

	nURI = n_elements(uri)

	;Breakdown the URLs
	self -> ParseURI, uri, $
	                  SCHEME = scheme, $
	                  HOST   = host, $
	                  PATH   = path
	
	;Extract the directory and file names
	basename = self -> Path_BaseName(path)
	dirname  = self -> Path_DirName(path)
	
	;Create the directory chain
	if nURI eq 1 then begin
		dir = filepath(basename, $
		               ROOT_DIR     = self.local_root, $
		               SUBDIRECTORY = [scheme, host, strsplit(dirname, '/', /EXTRACT)])
	endif else begin
		dir = strarr(nURI)
		for i = 0, nURI-1 do begin
			dir[i] = filepath(basename[i], $
			                  ROOT_DIR     = self.local_root, $
			                  SUBDIRECTORY = [scheme[i], host[i], dirname[i]])
		endfor
	endelse
	
	
	return, dir
end


;+
;   Callback function for the Download method.
;
; :Params:
;       STATUSINFO:         in, required, type=strarr
;                           Status information about the Get or Put methods.
;       PROGRESSINFO:       in, required, type=lon64arr
;                           Contains progress information. Elements::
;                               0 - 1=valid data, 0=invalid data
;                               1 - Download total for Get
;                               2 - Number of bytes downloaded
;                               3 - Upload total for Put
;                               4 - Number of bytes uploaded
;       CALLBACKDATA:       in, optional, type=structure
;                           A structure with the following tags::
;                               PROGRESSBAR - A cgProgressBar object
;                               PERCENT     - The percent downloaded
;
; :Returns:
;       CONTINUE:           1 if operation should continue, 0 if operation should
;                               be halted.
;-
function MrWebURI_Callback_Download, statusInfo, progressInfo, callbackData
	compile_opt idl2
	on_error, 2

	;Assume no cancel
	tf_cancel = 0

	;The response header does not contribute to the total download
	if progressInfo[2] gt 0 then begin
		;Determine the total size
		;   - ProgressInfo may or may not (for PHP) contain the total size
		;   - If it doesn't, try to get it from the header information
		if callbackData.total_size eq 0 || progressInfo[2] lt callbackData.last_size then begin
			;Check PROGRESSINFO
			if progressInfo[1] gt 0 then begin
				callbackData.total_size = progressInfo[1]
			
			;Parse from header
			endif else begin
				callbackData.oNet -> GetProperty, RESPONSE_HEADER=rh
				header = callbackData.oWeb -> ParseResponseHeader(rh)
				callbackData.total_size = fix(header.content_length, TYPE=14)
			endelse
			
			;Reset percent complete
			CallbackData.bar -> Update, 0.0
		endif

		;Percent complete
		pct      = double(progressInfo[2]) / double(CallbackData.total_size) * 100.0D
		pct_diff = pct - CallbackData.last_update

		;Did the percentage change by more than 0.5?
		if pct_diff gt 1.0 then begin
			;Did user cancel?
			tf_cancel = CallbackData.bar -> CheckCancel()

			;Update progress
			CallbackData.bar -> Update, pct
			CallbackData.last_update = pct
			CallbackData.oNet -> SetProperty, CALLBACK_DATA=CallbackData
		endif
	endif

	; return 1 to continue, return 0 to cancel
	if tf_cancel $
		then return, 0 $
		else return, 1
end


;+
;   Callback function for the GetResponseHeader method.
;
; :Params:
;       STATUSINFO:         in, required, type=strarr
;                           Status information about the Get or Put methods.
;       PROGRESSINFO:       in, required, type=lon64arr
;                           Contains progress information. Elements::
;                               0 - 1=valid data, 0=invalid data
;                               1 - Download total for Get
;                               2 - Number of bytes downloaded
;                               3 - Upload total for Put
;                               4 - Number of bytes uploaded
;       CALLBACKDATA:       in, optional, type=depends
;                           User data passed into the callback function.
;
; :Returns:
;       CONTINUE:           1 if operation should continue, 0 if operation should
;                               be halted.
;-
function MrWebURI_Callback_ResponseHeader, statusInfo, progressInfo, callbackData
	compile_opt idl2

	;The response header does not contribute to the download total.
	if (progressInfo[0] eq 1) && (progressInfo[2] gt 0) $
		then return, 0 $
		else return, 1
end


;+
;   Generic callback function for the IDLnetUR object. Passes everything through.
;
; :Params:
;       STATUSINFO:         in, required, type=strarr
;                           Status information about the Get or Put methods.
;       PROGRESSINFO:       in, required, type=lon64arr
;                           Contains progress information. Elements::
;                               0 - 1=valid data, 0=invalid data
;                               1 - Download total for Get
;                               2 - Number of bytes downloaded
;                               3 - Upload total for Put
;                               4 - Number of bytes uploaded
;       CALLBACKDATA:       in, optional, type=structure
;                           Data stored by the user in the IDLnetURL CALLBACKDATA property.
;
; :Returns:
;       CONTINUE:           1 if operation should continue, 0 if operation should
;                               be halted.
;-
function MrWebURI_Callback, statusInfo, progressInfo, callbackData
	return, 1
end


;+
;   Cleanup after the object is destroyed.
;-
pro MrWebURI::Cleanup
	compile_opt idl2
	on_error, 2

	;Superclasses
	self -> MrURI::Cleanup
	self -> IDLnetURL::Cleanup
end


;+
;   The initialization method.
;
; :Params:
;       URI:            in, optional, type=string
;                       File identifier.
;
; :Keywords:
;       DROPBOX_ROOT:       in, optional, type=string, default=MRWEBDATA_DROPBOX_ROOT environment variable
;                           Local folder in which files are temporarily placed before
;                               being moved to into `LOCAL_ROOT`.
;       LOCAL_ROOT:         in, optional, type=string, default=MRWEBDATA_LOCAL_ROOT environment variable
;                           Local directory root where downloaded files are to be saved.
;                               The underlying directory structure should mimic the remote
;                               site from which data was obtained.
;       MIRROR_ROOT:    in, optional, type=string, default=MRWEBDATA_MIRROR_ROOT environment variable
;                       Remote root directory of a data mirror site.
;       NO_DOWNLOAD:    in, optional, type=boolean, default=0
;                       If set, use only those files that are saved locally. Files are
;                           still searched for on the remote server. Remote files are
;                           normally downloaded if they are more recent and/or have a
;                           higher version number than local files.
;       OFFLINE:        in, optional, type=boolean, default=0
;                       If set, the object will function in offline mode (search for
;                           files locally). Automatically sets `NO_DOWNLOAD` to the same value.
;       REMOTE_ROOT:    in, optional, type=string, default=MRWEBDATA_REMOTE_ROOT environment variable
;                       Remote root directory where data is saved.
;       VERBOSE:        in, optional, type=boolean, default=0
;                       If set, status messages will be printed to standard output.
;
; :Returns:
;       If successful, a valid MrWebURI object will be returned.
;-
function MrWebURI::init, uri, $
DROPBOX_ROOT=dropbox_root, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
NO_DOWNLOAD=no_download, $
OFFLINE=offline, $
REMOTE_ROOT=remote_root, $
VERBOSE=verbose
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif

	;Create an IDLnetURL object
	success = self -> IDLnetURL::Init(CALLBACK_FUNCTION='MrWebURI_Callback')
	if success eq 0 then return, 0
	
	;Initialize superclasses
	;   - Must be initialized after the NetURL object
	success = self -> MrURI::Init(uri)
	if success eq 0 then return, 0
	
	;Search for environment variables
	if n_elements(dropbox_root) eq 0 then dropbox_root = getenv('MRWEBDATA_DROPBOX_ROOT')
	if n_elements(local_root)   eq 0 then local_root   = getenv('MRWEBDATA_LOCAL_ROOT')
	if n_elements(mirror_root)  eq 0 then mirror_root  = getenv('MRWEBDATA_MIRROR_ROOT')
	if n_elements(remote_root)  eq 0 then remote_root  = getenv('MRWEBDATA_REMOTE_ROOT')
	
	;Default local root
	if local_root eq '' then begin
		;The '~' returns the home directory
		local_root = filepath('MrWebData', ROOT_DIR=file_search('~', /EXPAND_TILDE))
		if ~file_test(local_root, /DIRECTORY) then file_mkdir, local_root
		
		;Inform user where data is being saved.
		MrPrintF, 'LogText', 'Setting local data root to "' + local_root + '".'
	endif
	
	;Set properties
	self -> SetProperty, OFFLINE      = offline, $
	                     NO_DOWNLOAD  = no_download, $
	                     DROPBOX_ROOT = dropbox_root, $
	                     LOCAL_ROOT   = local_root, $
	                     MIRROR_ROOT  = mirror_root, $
	                     REMOTE_ROOT  = remote_root, $
	                     VERBOSE      = verbose

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
;       OFFLINE:        Flag for working in offline mode
;       NO_DOWNLOAD:    Flag for using only local files
;       LOCAL_ROOT:     Local directory root where files are to be saved.
;       MIRROR_ROOT:    Root of remote mirror directory at which to find data.
;       REMOTE_ROOT:    Remote directory where data is stored.
;-
pro MrWebURI__Define, class
	compile_opt idl2

	class = { MrWebURI, $
	          inherits IDLnetURL, $
	          inherits MrURI, $
	          oFile:        obj_new(), $
	          offline:      0B, $
	          no_download:  0B, $
	          dropbox_root: '', $
	          local_root:   '', $
	          mirror_root:  '', $
	          remote_root:  '' $
	        }
end
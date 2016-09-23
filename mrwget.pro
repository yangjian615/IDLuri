; docformat = 'rst'
;
; NAME:
;       MrWGet
;
;*****************************************************************************************
;   Copyright (c) 2014, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   An IDL wrapper for the WGet command. Not all WGet options are available.
;
; :Examples:
;   Get help about WGet::
;       MrWGet, /HELP
;
;   Get the version of WGet installed::
;       MrWGet, /VERSION
;
;   Download a file to the current directory without any extra directories appended::
;       cd, CURRENT=pwd
;       url = 'http://www.google.com'
;       MrWGet, url, DIRECTORY_PREFIX=pwd, /NO_HOST_DIRECTORIES, /NO_PARENT, /SHOW_COMMAND
;
;   Download an array of URLs. Return the name of the temporary file list so that it
;   is not deleted automatically::
;       cd, CURRENT=pwd
;       url = ['http://www.rbsp-ect.lanl.gov/data_pub/rbspa/MagEphem/def/2013/rbspa_def_MagEphem_TS04D_20130101_v2.0.0.h5'
;              'http://www.rbsp-ect.lanl.gov/data_pub/rbspa/MagEphem/def/2013/rbspa_def_MagEphem_TS04D_20130102_v2.0.0.h5'
;              'http://www.rbsp-ect.lanl.gov/data_pub/rbspb/MagEphem/def/2013/rbspb_def_MagEphem_TS04D_20130101_v2.0.0.h5'
;              'http://www.rbsp-ect.lanl.gov/data_pub/rbspb/MagEphem/def/2013/rbspb_def_MagEphem_TS04D_20130102_v2.0.0.h5']
;       MrWGet, url, DIRECTORY_PREFIX=pwd, FILE_LIST=temp_file, $
;                    /NO_HOST_DIRECTORIES, /NO_PARENT, /SHOW_COMMAND
;       print, temp_file
;           /Users/argall/Documents/Work/Data/RBSP/Ephemeris/MrWGet_Temp_DowloadList.txt
;       file_delete, temp_file
;
; :Categories:
;       Download
;
; :Params:
;       URL:                in, required, type=string/strarr
;                           File from the web to be downloaded. If more than one URL is
;                               given, an attempt at creating a temporary `FILE_LIST` named
;                               MrWGet_Temp_DownloadList.txt will be made. A file will be
;                               created either in `DIRECTORY_PREFIX` (if given) or the
;                               current directory. Unless `FILE_LIST` is provided, the
;                               temporary file will be deleted after all URLs are
;                               downloaded. If creating the file list fails, Spawn will
;                               be called once per URL using multiple processes.
;
; :Keywords:
;       ACCEPT_LIST:        in, optional, type=string/strarr
;                           Accepted file extensions
;       APPEND_OUTPUT:      in, optional, type=string
;                           Name of a text file to which log messages will be appended.
;       ASK_PASSWORD:       in, optional, type=boolean, default=0
;                           If set, wget will ask for a password. In this case `PASSWORD`
;                               does not have to be specified.
;       BACKGROUND:         in, optional, type=boolean, default=0
;                           If set, WGet will go directly to background after it starts.
;                               If no output is specified via `OUTPUT` then output is
;                               redirected to MrWGet_Message_Log.txt and placed either in
;                               `DIRECTORY_PREFIX` or the current directory.
;       COMMANDS:           in, optional, type=string, default=''
;                           A string of user-supplied commands to be passed to WGet.
;       CONTINUE:           in, optional, type=boolean, default=0
;                           Resume getting a partially downloaded file.
;       CONVERT_HYPERLINKS: in, optional, type=boolean, default=0
;                           If set, hyperlinks are converted to local links that pages
;                               can be navigated from the local machine.
;       DIRECTORY_PREFIX:   in, optional, type=string, default=current directory
;                           Files will be saved to DIRECTORY_PREFIX/
;       FTP_USERNAME:       in, optional, type=string
;                           Username required for FTP client authentification.
;       FTP_PASSWORD:       in, optional, type=string
;                           Password required for FTP client authentification.
;       HELP:               in, optional, type=boolean, default=0
;                           Print help information for using WGet.
;       LEVEL:              in, optional, type=int
;                           Number of subdirectories to access when doing a `RECURSIVE` get.
;       LIMIT_RATE:         in, optional, type=int
;                           Set the download rate in kilobytes per second. The default
;                               is to use all available bandwidth.
;       MIRROR:             in, optional, type=boolean, default=0
;                           If set, the `URL` will be mirrored. Shortcut for::
;                               "wget -N -r -l inf --no-remove-listings URL"
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           Skip downloads that would overwrite existing files.
;       NO_DIRECTORIES:     in, optional, type=boolean, default=0
;                           Do not create directories. All files will be saved to the
;                               current directory. In the `NO_HOST_DIRECTORIES` example,
;                               the /wiki and /Wget directories would also be created.
;      NO_HOST_DIRECTORIES: in, optional, type=boolean, default=0
;                           Do not create host directories. For the site
;                               "http://en.wikipedia.org/wiki/Wget", a directory with the
;                               host name "en.wikipedia.org" would otherwise be created.
;       NO_PARENT:          in, optional, type=boolean, default=0
;                           If set, downloads will progress only down the directory chain.
;                               It will not ascend to the any parent directory.
;       NO_VERBOSE:         in, optional, type=boolean, default=0
;                           Turn off `VERBOSE` without being `QUIET`
;       OFILENAME:          in, optional, type=string
;                           Name of the file to which the download will be saved. The
;                               default is to use the same name as contained in `URL`.
;       OUTPUT:             in, optional, type=string
;                           Name of a text file to which messages will be logged.
;                               If `BACKGROUND` is specified, a message log will be
;                               created. Provide a named variable into which the path of
;                               the error log is returned to prevent the log from being
;                               deleted after download completes.
;       PASSWORD:           in, optional, type=string
;                           A password to accompany `USERNAME` when required. See also
;                               `ASK_PASSWORD`.
;       QUIET:              in, optional, type=boolean, default=0
;                           Do not output any progress information.
;       RECURSIVE:          in, optional, type=boolean, default=0
;                           Download recursively.
;       REJECT_LIST:        in, optional, type=string/strarr
;                           Extensions to reject.
;       SHOW_COMMAND:       in, optional, type=boolean, default=0
;                           Print the WGet command.
;       SPIDER:             in, optional, type=boolean, default=0
;                           Crawl the web and checks if `URL` exists. Nothing is downloaded.
;       TIME_STAMPING:      in, optional, type=boolean, default=0
;                           If set, files will only be downloaded if they are newer than
;                               local files.
;       TRIES:              in, optional, type=int
;                           Number of retries (0 = unlimited)
;       FILE_LIST:          in, optional, type=string, default=''
;                           A text file containing the URLs to download, each on its own
;                               line. If this is specified, `URL` is ignored. See the `URL`
;                               keyword for more details.
;       USERNAME:           in, optional, type=string
;                           Username, if required
;       VERBOSE:            in, optional, type=boolean
;                           If set, verbose progress information will be printed to the
;                               command window (this is the default). 
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/03/19  -   Written by Matthew Argall
;       2014/04/01  -   Added the CONVERT_HYPERLINKS, ERROR_LOG, FTP_USERNAME, $
;                           FTP_PASSWORD, LIMIT_RATE, and SPIDER keywords. - MRA
;       2014/06/06  -   Multiple URLs are now accepted. File lists are created. Error
;                           log is created when BACKGROUND is set. - MRA
;       2014/06/08  -   Renamed ERROR_LOG to OUTPUT and ERROR_APPEND to APPEND_OUTPUT.
;                           Added the COMMANDS keyword. - MRA
;-
pro MrWGet, url, $
ACCEPT_LIST=accept_list, $
ASK_PASSWORD=ask_password, $
APPEND_OUTPUT=append_output, $
BACKGROUND=background, $
COMMANDS=user_commands, $
CONTINUE=resume, $
CONVERT_HYPERLINKS=convert_hyperlinks, $
DIRECTORY_PREFIX=directory_prefix, $
FILE_LIST=file_list, $
FTP_USERNAME=ftp_username, $
FTP_PASSWORD=ftp_password, $
HELP=help, $
LEVEL=level, $
LIMIT_RATE=limit_rate, $
MIRROR=mirror, $
NO_CLOBBER=no_clobber, $
NO_DIRECTORIES=no_directories, $
NO_HOST_DIRECTORIES=no_host_directories, $
NO_PARENT=no_parent, $
NO_VERBOSE=no_verbose, $
OUTPUT=output, $
PASSWORD=password, $
QUIET=quiet, $
RECURSIVE=recursive, $
REJECT_LIST=reject_list, $
SHOW_COMMAND=show_command, $
SPIDER=spider, $
TIME_STAMPING=time_stamping, $
TRIES=tries, $
USERNAME=username, $
VERBOSE=verbose, $
VERSION=version, $
WAIT=PAUSE
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(lun) gt 0 then free_lun, lun
        if file_list_created && file_test(file_list)    then file_delete, file_list
        if create_error_log  && file_test(temp_err_log) then file_delete, temp_err_log
        void = cgErrorMsg()
        return
    endif
    
    ;Flag indicating if files were created.
    file_list_created = 0
    create_error_log  = 0
    
;---------------------------------------------------------------------
; Quickies ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;WGet version
    if keyword_set(version) then begin
        spawn, 'wget -V'
        return
    endif
    
    ;Help information
    if keyword_set(help) then begin
        spawn, 'wget -h'
        return
    endif
    
    ;Was a URL given?
    if n_elements(url) eq 0 then $
        message, 'Usage: IDL> MrWebGet, url'
    
;---------------------------------------------------------------------
; Defaults ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ask_password        = keyword_set(ask_password)
    background          = keyword_set(background)
    convert_hyperlinks  = keyword_set(convert_hyperlinks)
    mirror              = keyword_set(mirror)
    no_clobber          = keyword_set(no_clobber)
    no_directories      = keyword_set(no_directories)
    no_host_directories = keyword_set(no_host_directories)
    no_parent           = keyword_set(no_parent)
    no_verbose          = keyword_set(no_verbose)
    quiet               = keyword_set(quiet)
    recursive           = keyword_set(recursive)
    resume              = keyword_set(resume)
    spider              = keyword_set(spider)
    show_command        = keyword_set(show_command)
    time_stamping       = keyword_set(time_stamping)
    verbose             = keyword_set(verbose)
    
    ;The least verbose option is chosen by default.
    if no_verbose + quiet + verbose gt 1 then begin
        message, 'NO_VERBOSE, VERBOSE, and QUIET are mutually exclusive. Choosing least verbose option', /INFORMATIONAL
        if quiet eq 1 then begin
            verbose = 0
            no_verbose = 0
        endif else if no_verbose eq 1 then begin
            verbose = 0
        endif
    endif
    
;---------------------------------------------------------------------
; Setup the Command //////////////////////////////////////////////////
;---------------------------------------------------------------------
    command = 'wget'
    
    ;User-provided commands?
    if n_elements(user_commands) gt 0 then command += ' ' + user_commands
    
    ;Boolean parameters
    if ask_password        then command += ' --ask-password'
    if background          then command += ' -b'
    if convert_hyperlinks  then command += ' -k'
    if mirror              then command += ' -m'
    if no_clobber          then command += ' -nc'
    if no_directories      then command += ' -nd'
    if no_host_directories then command += ' -nH'
    if no_parent           then command += ' -np'
    if no_verbose          then command += ' -nv'
    if quiet               then command += ' -q'
    if recursive           then command += ' -r'
    if resume              then command += ' -c'
    if spider              then command += ' --spider'
    if time_stamping       then command += ' -N'
    if verbose             then command += ' -v'
    
    ;Non-boolean input
    if n_elements(accept_list)      gt 0 then command += ' -A '             + strjoin(accept_list, ',')
    if n_elements(append_output)    gt 0 then command += ' -a '             + append_output
    if n_elements(directory_prefix) gt 0 then command += ' -P '             + strjoin(directory_prefix, ',')
    if n_elements(ftp_username)     gt 0 then command += ' --ftp-user='     + ftp_username
    if n_elements(ftp_password)     gt 0 then command += ' --ftp-password=' + ftp_password
    if n_elements(level)            gt 0 then command += ' -l '             + string(level, FORMAT='(i0)')
    if n_elements(ofilename)        gt 0 then command += ' -O '             + ofilename
    if n_elements(output)           gt 0 then command += ' -o '             + output
    if n_elements(pause)            gt 0 then command += ' -w '             + string(pause, FORMAT='(i0)')
    if n_elements(password)         gt 0 then command += ' --password='     + password
    if n_elements(reject_list)      gt 0 then command += ' -R '             + strjoin(reject_list, ',')
    if n_elements(tries)            gt 0 then command += ' -t '             + string(tries, FORMAT='(i0)')
    if n_elements(username)         gt 0 then command += ' --user='         + username
    if n_elements(limit_rate)       gt 0 then command += ' --limit-rate='   + string(limit_rate, FORMAT='(i0)') + 'k'

;---------------------------------------------------------------------
; File List for >1 URL ///////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Find a writable directory
    if n_elements(directory_prefix) gt 0 $
        then write_dir = directory_prefix $
        else cd, CURRENT=write_dir
            
    ;If more than one file was given, try to create a file list
    nURL = n_elements(url)
    if nURL gt 1 then begin
        ;Indicate that a file list was created
        file_list_created = 1
    
        ;Open a temporary file.
        file_list = filepath('MrWGet_Temp_DowloadList.txt', ROOT_DIR=write_dir)
        openw, lun, file_list, /GET_LUN
        if quiet eq 0 then message, 'Download list written to: "' + file_list + '".', /INFORMATIONAL
        
        ;Write URL names to the file
        for i = 0, nURL-1 do printf, lun, url[i]
        
        ;Close the file
        free_lun, lun
    endif
    
;---------------------------------------------------------------------
; Error Log //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Create an error log if none was provided
    if background && n_elements(error_log) eq 0 && n_elements(error_append) eq 0 then begin
        ;Name of log file
        temp_err_log = filepath('MrWGet_Message_Log.txt', ROOT_DIR=write_dir)
        
        ;If the log file does not exist, create it.
        if file_test(temp_err_log) eq 0 then begin
            create_error_log = 1
            openw, lun, temp_err_log, /GET_LUN
            free_lun, lun
        endif
        
        ;Add to the command
        command += ' -a ' + temp_err_log
        if quiet eq 0 then message, 'Output will be written to: "' + temp_err_log + '".', /INFORMATIONAL
    endif

;---------------------------------------------------------------------
; Call WGet //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Create the final command
    if n_elements(file_list) eq 0 $
        then command += ' ' + url $
        else command += ' -i ' + file_list
        
    ;Show the command?
    if show_command then print, command

    ;Spawn for each requested URL
    nCMD = n_elements(command)
    if nCMD eq 1 $
        then spawn, command $
        else for i = 0, nCMD - 1 do spawn, command[i]
    
;---------------------------------------------------------------------
; Clean Up ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Delete the temporary file list?
    if file_list_created && arg_present(file_list) eq 0 $
        then if background eq 0 then file_delete, file_list
    
    ;Delete the error log?
    if create_error_log then begin
        if arg_present(error_log) $
            then error_log = temp_err_log $
            else if background eq 0 then file_delete, temp_err_log
    endif
end
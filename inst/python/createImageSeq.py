import os
import sys
import subprocess
import string
import argparse

### Defaults and constants

# moviepath_default = 'Movies'
# imagepath_default = 'ImageSequences'
# x_default = 1920
# y_default = 1080
# fps_default = 15
# nsec_default = 3
start_default= 0
stop_default = sys.maxint
# ext_default = ['mts']
nodate_prefix = 'nodate_'
debug = False

def conv_command(fileName, fps, hor, vert, targetDirName, start_given, stop_given):
	inFile = os.path.join(movieDir, fileName)
	outFiles = os.path.join(sequenceParentDir, targetDirName, 'image-%03d.png')
	return [libavpath,
			'-loglevel', 'quiet',
			'-i', inFile,
			'-r', str(fps),
			'-s', str(hor) + 'x' + str(vert),
			'-ss', str(start_given),
			'-t', str(stop_given - start_given),
			'-f', 'image2',
			outFiles]		
		
def which(cmd, mode=os.F_OK | os.X_OK, path=None):
    """Given a command, mode, and a PATH string, return the path which
    conforms to the given mode on the PATH, or None if there is no such
    file.

    `mode` defaults to os.F_OK | os.X_OK. `path` defaults to the result
    of os.environ.get("PATH"), or can be overridden with a custom search
    path.
    
	Copied from 3.6 source referred to at https://docs.python.org/3.6/library/shutil.html,
	also works for 2.7.
    """
    # Check that a given file can be accessed with the correct mode.
    # Additionally check that `file` is not a directory, as on Windows
    # directories pass the os.access check.
    def _access_check(fn, mode):
        return (os.path.exists(fn) and os.access(fn, mode)
                and not os.path.isdir(fn))

    # If we're given a path with a directory part, look it up directly rather
    # than referring to PATH directories. This includes checking relative to the
    # current directory, e.g. ./script
    if os.path.dirname(cmd):
        if _access_check(cmd, mode):
            return cmd
        return None

    if path is None:
        path = os.environ.get("PATH", os.defpath)
    if not path:
        return None
    path = path.split(os.pathsep)

    if sys.platform == "win32":
        # The current directory takes precedence on Windows.
        if not os.curdir in path:
            path.insert(0, os.curdir)

        # PATHEXT is necessary to check on Windows.
        pathext = os.environ.get("PATHEXT", "").split(os.pathsep)
        # See if the given file matches any of the expected path extensions.
        # This will allow us to short circuit when given "python.exe".
        # If it does match, only test that one, otherwise we have to try
        # others.
        if any(cmd.lower().endswith(ext.lower()) for ext in pathext):
            files = [cmd]
        else:
            files = [cmd + ext for ext in pathext]
    else:
        # On other platforms you don't have things like PATHEXT to tell you
        # what file suffixes are executable, so just pass on cmd as-is.
        files = [cmd]

    seen = set()
    for dir in path:
        normdir = os.path.normcase(dir)
        if not normdir in seen:
            seen.add(normdir)
            for thefile in files:
                name = os.path.join(dir, thefile)
                if _access_check(name, mode):
                    return name
    return None
			
parser = argparse.ArgumentParser()

parser.add_argument('-moviepath')
parser.add_argument('-imagepath')
parser.add_argument('-libavpath')
parser.add_argument('-exiftoolpath')
parser.add_argument('-x')
parser.add_argument('-y')
parser.add_argument('-fps')
parser.add_argument('-nsec')
parser.add_argument('-start')
parser.add_argument('-stop')
parser.add_argument('-ext', nargs='*')
parser.add_argument('-verbose')

args = parser.parse_args()

# process arguments


libavpath = args.libavpath
exiftoolpath = args.exiftoolpath

avconv = which(libavpath)
exiftool = which(exiftoolpath)
 
if avconv:
	if debug:
		print 'Using avconv found at %s' % avconv
else:
	print 'Cannot find avconv executable, specify with "libavpath="'
	print 'Cannot continue, aborting execution'
	exit(1)
	
if exiftool:
	if debug:
		print 'Using exiftool found at %s' % which('exiftool')
else:
	print 'Cannot find exiftool executable, specify with "exiftoolpath="'
	print 'Falling back to defaults for folder names and duration'

if args.moviepath == 'NULL':
	print 'moviepath must not be NULL. Stop'
	exit(1)
else:
	moviepath = args.moviepath
	
if args.imagepath == 'NULL':
	print 'imagepath must not be NULL. Stop'
	exit(1)
else:
	imagepath = args.imagepath
	
if args.x == 'NULL':
	print 'x must not be NULL. Stop'
	exit(1)
else:
	x = args.x

if args.y == 'NULL':
	print 'y must not be NULL. Stop'
	exit(1)
else:
	y = args.y

if args.fps == 'NULL':
	print 'fps must not be NULL. Stop'
	exit(1)
else:
	fps = args.fps

if args.nsec == 'NULL':
	nsec = None
else:
	nsec = float(args.nsec)

if args.start == 'NULL':
	start_given = None
else:
	start_given = float(args.start)

if args.stop == 'NULL':
	stop_given = None
else:
	stop_given = float(args.stop)

if args.ext == ['NULL']:
	print 'ext must not be NULL. Stop'
	exit(1)
else:
	extensions = args.ext
	
if args.verbose not in ['FALSE', 'TRUE']:
	print 'Unknown value %s for verbose. Converting to FALSE' % args.verbose
	verbose = False
else:
	verbose = args.verbose == 'TRUE'
		
if start_given and stop_given and not start_given < stop_given:
	print 'Error: start (%.2f) must be smaller than stop (%.2f). Stop.' % (start_given, stop_given)
	exit(1)
	
if start_given and start_given < 0:
	print 'Error: start must not be negative. Stop.'
	exit(1)

if verbose:
	print 'createImageSec v1.2'

movieDir = os.path.abspath(moviepath)
sequenceParentDir = os.path.abspath(imagepath)	
					
if debug:
	print 'Running script with values: localdir: %s, moviedir: ./%s, sequencedir: ./%s, x: %s, y: %s, fps: %s, nsec: %s, start_given: %s, stop_given: %s, extensions: %s' % (
		os.getcwd(), moviepath, imagepath, x, y, fps, nsec, start_given, stop_given, extensions
		)					
					
def getDuration(filePath):
	if exiftool:
		try:
			duration = float(string.translate(subprocess.check_output([
				exiftool,
				'-api', 'LargeFileSupport',
				'-n',
				'-s3',
				'-duration',
				filePath
				], stderr=open('/dev/null')).split()[0], None, ':'))
		except subprocess.CalledProcessError as e:
			if debug:
				print 'Error in determining duration for file %s: %s' % (filePath, e)
			duration = None
	else:
		duration = None
	return duration

def getTargetDirNamePrefix(fileName):
	if exiftool:
		try:
			targetDirNamePrefixRaw = subprocess.check_output([
				exiftool,
				'-api', 'LargeFileSupport',
				'-DateTimeOriginal',
				'-T',
				os.path.join(movieDir, fileName)
			], stderr=open('/dev/null')).strip().strip('-') # Note: '-' is sometimes returned by exiftool if DateTimeOriginal field empty
			targetDirNamePrefix = string.translate(targetDirNamePrefixRaw.split()[0], None, ':') + '_'
		except subprocess.CalledProcessError as e:
			if debug:
				print 'Error in determining date for file %s: %s' % (os.path.join(movieDir, fileName), e)		
				print 'Got raw prefix: "%s"' % targetDirNamePrefixRaw
			targetDirNamePrefix = nodate_prefix
		if not len(targetDirNamePrefix) == 8 and targetDirNamePrefix.isdigit():
			targetDirNamePrefix = nodate_prefix
		return targetDirNamePrefix
	else:
		return nodate_prefix


movieNames = []
for fileName in os.listdir(movieDir):
	movieName, movieExtension = os.path.splitext(fileName)
	if not (os.path.isfile(os.path.join(movieDir, fileName))
			and movieExtension[1:].lower() in extensions):
		if debug:
			print 'File %s has the wrong name or is a directory, skipping file.' % fileName
	else:
		movieNames.append(fileName)
		
if not os.path.exists(sequenceParentDir):
	os.mkdir(sequenceParentDir)

if start_given and stop_given and nsec:
	if verbose:
		print 'Warning - both start and stop given: nsec ignored.' 

for fileName in movieNames:
	movieName, movieExtension = os.path.splitext(fileName)

	targetDirNamePrefix = getTargetDirNamePrefix(fileName)
	targetDirName = targetDirNamePrefix + fileName

	if os.path.exists(os.path.join(sequenceParentDir, targetDirName)):
		if verbose:
			print 'Folder %s already exists, file %s skipped' % (targetDirName, fileName)
	else:
		if targetDirNamePrefix == nodate_prefix:
			if verbose:
				print 'File %s: exiftool cannot determine date.' % fileName	
			
		if start_given:
			filestart = start_given
		else:
			filestart = 0
			
		if stop_given:
			filestop = stop_given
		else:
			filestop = sys.maxint
				
		duration = getDuration(os.path.join(movieDir, fileName))
		if debug:
			print 'Duration %s found for file %s.' % (duration, fileName)
		if not duration:
			if verbose:
				print 'File %s: exiftool cannot determine length.' % fileName
		if start_given and stop_given:
			pass
		elif start_given:
			if nsec:
				filestop = filestart + nsec
		elif stop_given:
			if nsec:
				filestart = filestop - nsec
		else:
			if nsec:
				if duration:
					filestart = duration/2 - nsec/2
					filestop = duration/2 + nsec/2
				else:
					filestop = nsec
		if duration and not filestart < duration:
			if verbose:
				print 'File %s: Start (%.2f) should be smaller dan duration (%.2f sec). Skipping video.' % (fileName, filestart, duration)
			continue
			
		filestart = max(filestart, 0)
		if duration and  filestop > duration:
			if verbose:
				print 'File %s: Requested end past end of file, setting end to %.2f' % (fileName, duration)
			filestop = min(filestop, duration)
			
		if filestop - filestart < nsec:
			if verbose:
				print 'File %s: Cannot convert requested duration (%.2f sec), converting %.2f sec.' % (fileName, nsec, filestop-filestart)
				
		command = conv_command(fileName, fps, x, y, targetDirName, filestart, filestop)
		if debug:
			print 'Command: %s' % command

		os.mkdir(os.path.join(sequenceParentDir, targetDirName))

		try:
			subprocess.call(command)
			if duration:
				if verbose:
					print 'File %s: Converting with start: %.2f, stop: %.2f to folder %s.' % (fileName, filestart, filestop, targetDirName)
			else:
				if verbose:
					print 'File %s: Attempting to convert with start: %.2f, stop: %.2f to folder %s.' % (fileName, filestart, filestop, targetDirName)
		except Exception as e:
			print 'Error in processing file %s: %s' % (fileName, e)

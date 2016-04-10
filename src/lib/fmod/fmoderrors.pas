(* ============================================================================================== *)
(* FMOD Ex - Error string header file. Copyright (c), Firelight Technologies Pty, Ltd. 2004-2008. *)
(*                                                                                                *)
(* Use this header if you want to store or display a string version / english explanation of      *)
(* the FMOD error codes.                                                                          *)
(*                                                                                                *)
(* ============================================================================================== *)

unit fmoderrors;

{$I fmod.inc}

interface

uses
  fmodtypes;

function FMOD_ErrorString(errcode: FMOD_RESULT): PChar;

implementation

function FMOD_ErrorString(errcode: FMOD_RESULT): PChar;
begin
  case errcode of
    FMOD_ERR_ALREADYLOCKED:          Result := 'Tried to call lock a second time before unlock was called. ';
    FMOD_ERR_BADCOMMAND:             Result := 'Tried to call a function on a data type that does not allow this type of functionality (ie calling Sound::lock on a streaming sound). ';
    FMOD_ERR_CDDA_DRIVERS:           Result := 'Neither NTSCSI nor ASPI could be initialised. ';
    FMOD_ERR_CDDA_INIT:              Result := 'An error occurred while initialising the CDDA subsystem. ';
    FMOD_ERR_CDDA_INVALID_DEVICE:    Result := 'Couldnt find the specified device. ';
    FMOD_ERR_CDDA_NOAUDIO:           Result := 'No audio tracks on the specified disc. ';
    FMOD_ERR_CDDA_NODEVICES:         Result := 'No CD/DVD devices were found. ';
    FMOD_ERR_CDDA_NODISC:            Result := 'No disc present in the specified drive. ';
    FMOD_ERR_CDDA_READ:              Result := 'A CDDA read error occurred. ';
    FMOD_ERR_CHANNEL_ALLOC:          Result := 'Error trying to allocate a channel. ';
    FMOD_ERR_CHANNEL_STOLEN:         Result := 'The specified channel has been reused to play another sound. ';
    FMOD_ERR_COM:                    Result := 'A Win32 COM related error occured. COM failed to initialize or a QueryInterface failed meaning a Windows codec or driver was not installed properly. ';
    FMOD_ERR_DMA:                    Result := 'DMA Failure.  See debug output for more information. ';
    FMOD_ERR_DSP_CONNECTION:         Result := 'DSP connection error.  Connection possibly caused a cyclic dependancy.  Or tried to connect a tree too many units deep (more than 128). ';
    FMOD_ERR_DSP_FORMAT:             Result := 'DSP Format error.  A DSP unit may have attempted to connect to this network with the wrong format. ';
    FMOD_ERR_DSP_NOTFOUND:           Result := 'DSP connection error.  Couldnt find the DSP unit specified. ';
    FMOD_ERR_DSP_RUNNING:            Result := 'DSP error.  Cannot perform this operation while the network is in the middle of running.  This will most likely happen if a connection or disconnection is attempted in a DSP callback. ';
    FMOD_ERR_DSP_TOOMANYCONNECTIONS: Result := 'DSP connection error.  The unit being connected to or disconnected should only have 1 input or output. ';
    FMOD_ERR_EVENT_ALREADY_LOADED:   Result := 'The specified project has already been loaded. Having multiple copies of the same project loaded simultaneously is forbidden. ';
    FMOD_ERR_EVENT_FAILED:           Result := 'An Event failed to be retrieved, most likely due to just fail being specified as the max playbacks behavior. ';
    FMOD_ERR_EVENT_GUIDCONFLICT:     Result := 'An event with the same GUID already exists. ';
    FMOD_ERR_EVENT_INFOONLY:         Result := 'Cant execute this command on an EVENT_INFOONLY event. ';
    FMOD_ERR_EVENT_INTERNAL:         Result := 'An error occured that wasnt supposed to.  See debug log for reason. ';
    FMOD_ERR_EVENT_MAXSTREAMS:       Result := 'Event failed because Max streams was hit when FMOD_EVENT_INIT_FAIL_ON_MAXSTREAMS was specified. ';
    FMOD_ERR_EVENT_MISMATCH:         Result := 'FSB mismatches the FEV it was compiled with, the stream/sample mode it was meant to be created with was different, or the FEV was built for a different platform. ';
    FMOD_ERR_EVENT_NAMECONFLICT:     Result := 'A category with the same name already exists. ';
    FMOD_ERR_EVENT_NEEDSSIMPLE:      Result := 'Tried to call a function on a complex event thats only supported by simple events. ';
    FMOD_ERR_EVENT_NOTFOUND:         Result := 'The requested event, event group, event category or event property could not be found. ';
    FMOD_ERR_FILE_BAD:               Result := 'Error loading file. ';
    FMOD_ERR_FILE_COULDNOTSEEK:      Result := 'Couldnt perform seek operation.  This is a limitation of the medium (ie netstreams) or the file format. ';
    FMOD_ERR_FILE_DISKEJECTED:       Result := 'Media was ejected while reading. ';
    FMOD_ERR_FILE_EOF:               Result := 'End of file unexpectedly reached while trying to read essential data (truncated data?). ';
    FMOD_ERR_FILE_NOTFOUND:          Result := 'File not found. ';
    FMOD_ERR_FILE_UNWANTED:          Result := 'Unwanted file access occured. ';
    FMOD_ERR_FORMAT:                 Result := 'Unsupported file or audio format. ';
    FMOD_ERR_HTTP:                   Result := 'A HTTP error occurred. This is a catch-all for HTTP errors not listed elsewhere. ';
    FMOD_ERR_HTTP_ACCESS:            Result := 'The specified resource requires authentication or is forbidden. ';
    FMOD_ERR_HTTP_PROXY_AUTH:        Result := 'Proxy authentication is required to access the specified resource. ';
    FMOD_ERR_HTTP_SERVER_ERROR:      Result := 'A HTTP server error occurred. ';
    FMOD_ERR_HTTP_TIMEOUT:           Result := 'The HTTP request timed out. ';
    FMOD_ERR_INITIALIZATION:         Result := 'FMOD was not initialized correctly to support this function. ';
    FMOD_ERR_INITIALIZED:            Result := 'Cannot call this command after System::init. ';
    FMOD_ERR_INTERNAL:               Result := 'An error occured that wasnt supposed to.  Contact support. ';
    FMOD_ERR_INVALID_ADDRESS:        Result := 'On Xbox 360, this memory address passed to FMOD must be physical, (ie allocated with XPhysicalAlloc.) ';
    FMOD_ERR_INVALID_FLOAT:          Result := 'Value passed in was a NaN, Inf or denormalized float. ';
    FMOD_ERR_INVALID_HANDLE:         Result := 'An invalid object handle was used. ';
    FMOD_ERR_INVALID_PARAM:          Result := 'An invalid parameter was passed to this function. ';
    FMOD_ERR_INVALID_POSITION:       Result := 'An invalid seek position was passed to this function. ';
    FMOD_ERR_INVALID_SPEAKER:        Result := 'An invalid speaker was passed to this function based on the current speaker mode. ';
    FMOD_ERR_INVALID_SYNCPOINT:      Result := 'The syncpoint did not come from this sound handle. ';
    FMOD_ERR_INVALID_VECTOR:         Result := 'The vectors passed in are not unit length, or perpendicular. ';
    FMOD_ERR_IRX:                    Result := 'PS2 only.  fmodex.irx failed to initialize.  This is most likely because you forgot to load it. ';
    FMOD_ERR_MAXAUDIBLE:             Result := 'Reached maximum audible playback count for this sounds soundgroup. ';
    FMOD_ERR_MEMORY:                 Result := 'Not enough memory or resources. ';
    FMOD_ERR_MEMORY_CANTPOINT:       Result := 'Cant use FMOD_OPENMEMORY_POINT on non PCM source data, or non mp3/xma/adpcm data if FMOD_CREATECOMPRESSEDSAMPLE was used. ';
    FMOD_ERR_MEMORY_IOP:             Result := 'PS2 only.  Not enough memory or resources on PlayStation 2 IOP ram. ';
    FMOD_ERR_MEMORY_SRAM:            Result := 'Not enough memory or resources on console sound ram. ';
    FMOD_ERR_MUSIC_UNINITIALIZED:    Result := 'Music system is not initialized probably because no music data is loaded. ';
    FMOD_ERR_NEEDS2D:                Result := 'Tried to call a command on a 3d sound when the command was meant for 2d sound. ';
    FMOD_ERR_NEEDS3D:                Result := 'Tried to call a command on a 2d sound when the command was meant for 3d sound. ';
    FMOD_ERR_NEEDSHARDWARE:          Result := 'Tried to use a feature that requires hardware support.  (ie trying to play a VAG compressed sound in software on PS2). ';
    FMOD_ERR_NEEDSSOFTWARE:          Result := 'Tried to use a feature that requires the software engine.  Software engine has either been turned off, or command was executed on a hardware channel which does not support this feature. ';
    FMOD_ERR_NET_CONNECT:            Result := 'Couldnt connect to the specified host. ';
    FMOD_ERR_NET_SOCKET_ERROR:       Result := 'A socket error occurred.  This is a catch-all for socket-related errors not listed elsewhere. ';
    FMOD_ERR_NET_URL:                Result := 'The specified URL couldnt be resolved. ';
    FMOD_ERR_NET_WOULD_BLOCK:        Result := 'Operation on a non-blocking socket could not complete immediately. ';
    FMOD_ERR_NOTREADY:               Result := 'Operation could not be performed because specified sound/DSP connection is not ready. ';
    FMOD_ERR_OUTPUT_ALLOCATED:       Result := 'Error initializing output device, but more specifically, the output device is already in use and cannot be reused. ';
    FMOD_ERR_OUTPUT_CREATEBUFFER:    Result := 'Error creating hardware sound buffer. ';
    FMOD_ERR_OUTPUT_DRIVERCALL:      Result := 'A call to a standard soundcard driver failed, which could possibly mean a bug in the driver or resources were missing or exhausted. ';
    FMOD_ERR_OUTPUT_ENUMERATION:     Result := 'Error enumerating the available driver list. List may be inconsistent due to a recent device addition or removal. ';
    FMOD_ERR_OUTPUT_FORMAT:          Result := 'Soundcard does not support the minimum features needed for this soundsystem (16bit stereo output). ';
    FMOD_ERR_OUTPUT_INIT:            Result := 'Error initializing output device. ';
    FMOD_ERR_OUTPUT_NOHARDWARE:      Result := 'FMOD_HARDWARE was specified but the sound card does not have the resources necessary to play it. ';
    FMOD_ERR_OUTPUT_NOSOFTWARE:      Result := 'Attempted to create a software sound but no software channels were specified in System::init. ';
    FMOD_ERR_PAN:                    Result := 'Panning only works with mono or stereo sound sources. ';
    FMOD_ERR_PLUGIN:                 Result := 'An unspecified error has been returned from a 3rd party plugin. ';
    FMOD_ERR_PLUGIN_INSTANCES:       Result := 'The number of allowed instances of a plugin has been exceeded. ';
    FMOD_ERR_PLUGIN_MISSING:         Result := 'A requested output, dsp unit type or codec was not available. ';
    FMOD_ERR_PLUGIN_RESOURCE:        Result := 'A resource that the plugin requires cannot be found. (ie the DLS file for MIDI playback) ';
    FMOD_ERR_PRELOADED:              Result := 'The specified sound is still in use by the event system, call EventSystem::unloadFSB before trying to release it. ';
    FMOD_ERR_RECORD:                 Result := 'An error occured trying to initialize the recording device. ';
    FMOD_ERR_REVERB_INSTANCE:        Result := 'Specified Instance in FMOD_REVERB_PROPERTIES couldnt be set. Most likely because it is an invalid instance number, or another application has locked the EAX4 FX slot. ';
    FMOD_ERR_SUBSOUNDS:              Result := 'The error occured because the sound referenced contains subsounds.  The operation cannot be performed on a parent sound, or a parent sound was played without setting up a sentence first. ';
    FMOD_ERR_SUBSOUND_ALLOCATED:     Result := 'This subsound is already being used by another sound, you cannot have more than one parent to a sound.  Null out the other parents entry first. ';
    FMOD_ERR_SUBSOUND_CANTMOVE:      Result := 'Shared subsounds cannot be replaced or moved from their parent stream, such as when the parent stream is an FSB file. ';
    FMOD_ERR_SUBSOUND_MODE:          Result := 'The subsounds mode bits do not match with the parent sounds mode bits.  See documentation for function that it was called with. ';
    FMOD_ERR_TAGNOTFOUND:            Result := 'The specified tag could not be found or there are no tags. ';
    FMOD_ERR_TOOMANYCHANNELS:        Result := 'The sound created exceeds the allowable input channel count.  This can be increased using the maxinputchannels parameter in System::setSoftwareFormat. ';
    FMOD_ERR_UNIMPLEMENTED:          Result := 'Something in FMOD hasnt been implemented when it should be! contact support! ';
    FMOD_ERR_UNINITIALIZED:          Result := 'This command failed because System::init or System::setDriver was not called. ';
    FMOD_ERR_UNSUPPORTED:            Result := 'A command issued was not supported by this object.  Possibly a plugin without certain callbacks specified. ';
    FMOD_ERR_UPDATE:                 Result := 'An error caused by System::update occured. ';
    FMOD_ERR_VERSION:                Result := 'The version number of this file format is not supported. ';
    FMOD_OK:                         Result := 'No errors.';
    else
      Result := 'Unknown error.';
  end;
end;

end.

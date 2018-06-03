
Working on getting the threading right for the nglview.

In python each NGLView widget has a  handle_msg_thread thread and a RemoveCallThread

They both get created in the __init__ function.

The RemoteCallThread initializes itself with the NGLView widget and two functions: "loadFile" and "replaceStructure".  These functions are special in that the RemoveCallThread waits until they are finished.
I have the thread recognize a special queue entry :shutdown - to shutdown the thread.


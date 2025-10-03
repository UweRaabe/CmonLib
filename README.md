# CmonLib
## My Common Library _(c'est mon)_

A collection of several units with different purpose.

**Cmon.AsyncGuard** provides basic classes to execute a task in parallel synchonizing with the VCL, while taking care of the usual pitfalls with dangling pointers and ownership.

**Cmon.CmdLineHandler** and **Cmon.CmdLineFileHandler** contain classes for handling command line applications. Derived classes concentrate on the actual task to do.

**Cmon.DataSense** adds data sensitivity to controls that don't support that itself. Place a `TDataSense` component onto a form and all supported controls get a `DataSource` and `DataField` property in the object inspector.

**Cmon.DataSetHelper** offers a class helper for `TDataSet` with enumerator support as well as loading and storing classes and records.

**Cmon.DataStorage** allows persistance of fields and properties of class instances, especially forms and frames, by applying attributes.

**Cmon.Dialogs** abstracts standard dialogs from any framework. Built upon **Cmon.Messaging**.

**Cmon.Factory** provides generic factories for classes and interfaces.

**Cmon.Initializing** establishes a way to control initialize code for other units into the call to `Application.Initialize`. This allows to make adjustments before, which would be near to impossible if the initialize code would execute in the units initialization section.

**Cmon.Logging** adds a simple, abstract logging mechanism. Built upon **Cmon.Messaging**.

**Cmon.Messaging** provides some bases for using `System.Messaging`.

**Cmon.MRU.Vcl** provides `TMRUFiles`, a component to be linked to a PopupMenu or single menu item doing the bookkeeping for most recently yused files.

**Cmon.Observers** introduces some helper classes for leveraging the observer support of several VCL controls as well as implement observer support for other classes.

**Cmon.Utilities** contains some class helpers and general purpose routines.

Most of the units are self-contained or form small clusters. This is to avoid when using one unit of the library having a bunch of others used indirectly. There are just a handful of units providing functionality used by several others.

- The helpers for `TComponent` and `TCollection` as well as some methods from `TUtilities` in _Cmon.Utilities_ are used in several places.
- Auto-registering of messaging handlers makes use of _Cmon.Initializing_ to allow some fine control without touching the initialization sections.
- The classes of _Cmon.Messaging_ come in too handy as that they were not preferred by those messaging handlers just mentioned.

None of these three units has any dependency on another _CmonLib_ unit nor on any other unit outside the _System_ namespace.

The other self-contained units are _Cmon.AsyncGuard_ and _Cmon.DataSetHelper_, where the latter obviously has a dependency on _Data.DB_. The rest form those individual clusters with dependencies on at most _Cmon.Messaging_, _Cmon.Initializing_ and _Cmon.Utilities_.

A couple of these _CmonLib_ units have their roots in either an article on [my blog](https://www.uweraabe.de/Blog/) or one of my sessions at the various [CodeRage DE](https://www.youtube.com/@EmbarcaderoGermany/search?query=CodeRage%20Uwe%20Raabe) events.
- AsyncGuard: [Async Tasks in VCL Projects](https://www.uweraabe.de/Blog/2021/11/07/async-tasks-in-vcl-projects/)
- DataSetHelper: [Dataset Enumerator Reloaded](https://www.uweraabe.de/Blog/2017/02/09/dataset-enumerator-reloaded/) and [Poor Man’s CSV Export](https://www.uweraabe.de/Blog/2013/11/06/poor-mans-csv-export/)
- DataStorage: [A Tribute To Attributes](https://youtu.be/7QQCEuZxFnE?feature=shared) (German)
- Observers: [Observer - simpel und elegant](https://youtu.be/eLzr6DStsPY?feature=shared) (German)
- Messaging, Dialogs, Logging: [Pimp My System Messaging](https://youtu.be/e7BIv-lrQFA?feature=shared) (German)
- VirtualImageLists (in _Cmon.Vcl.Forms_): [ImageLists and High DPI](https://www.uweraabe.de/Blog/2022/09/19/imagelists-and-high-dpi/)

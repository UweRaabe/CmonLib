# CmonLib
## My Common Library _(c'est mon)_

A collection of several units with different purpose.

**Cmon.CmdLineHandler** and **Cmon.CmdLineFileHandler** contain classes for handling command line applications. Derived classes concentrate on the actual task to do.

**Cmon.DataSense** adds data sensitivity to controls that don't support that itself. Place a _TDataSense_ component onto a form and all supported controls get a _DataSource_ and _DataField_ property in the object inspector.

**Cmon.DataSetHelper** offers a class helper for TDataSet with enumerator support as well as loading and storing classes and records.

**Cmon.DataStorage** allows persistance of fields and properties of class instances, especially forms and frames, by applying attributes.

**Cmon.Dialogs** abstracts standard dialogs from any framework. Built upon **Cmon.Messaging**.

**Cmon.Initializing** establishes a way to control initialize code for other units into the call to _Application.Initialize_. This allows to make adjustments before, which would be near to impossible if the initialize code would execute in the units initialization section.

**Cmon.Logging** adds a simple, abstract logging mechanism. Built upon **Cmon.Messaging**.

**Cmon.Messaging** provides some bases for using _System.Messaging_.

**Cmon.Observers** introduces some helper classes for leveraging the observer support of several VCL controls as well as implement observer support for other classes.

**Cmon.Utilities** contains some class helpers and general purpose routines.

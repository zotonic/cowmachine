# Project Overview

This project is an interface layer and abstraction of the HTTP protocol for Erlang web
applications. The content management system Zotonic is the primary user of this library.

Applications using this library implement controllers with callback functions for
the various stages of HTTP requests. Cowmachine will call the callbacks in the correct
order and with the correct parameters, and will handle the low level details of the HTTP
protocol and connection management. The callbacks can be used to implement the application
logic for handling requests and generating responses.

## Folder Structure

- `/doc`: Contains generated edoc.
- `/docs`: Contains documentation about the flow of requests and the callbacks. 
- `/examples`: Contains examples for the use of Cowmachine.
- `/src`: Contains the source code for the Cowmachine.
- `/test`: Contains the tests.

## Libraries and Frameworks

- [Erlang](https://erlang.org) for controllers, models and module code.
- [Cowlib](https://github.com/ninenines/cowlib) and [Cowboy](https://github.com/ninenines/cowboy) for the
  low level HTTP connection handling.
- [Zotonic standard library](https://github.com/zotonic/z_stdlib) for support routines.

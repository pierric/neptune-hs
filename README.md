## Neptune high-level API

```haskell
main = do
    -- Experiment 'sandbox' must be created from the Neptune dashboard
    withNept "jiasen/sandbox" $ \_ experiment -> do
        forM_ [1..10::Int] $ \i -> do
            -- You can log arbitrary name/value (current limited to double values)
            nlog experiment "counter" (fromIntegral (i * i) :: Double)
            threadDelay 1000000
```

## TODOs
* Support logging text, image value.
* Create project
* Support system channels
* Get git information

## OpenAPI Auto-Generated http-client Bindings to Neptune Backend API

This library is intended to be imported qualified.

### Modules

| MODULE              | NOTES                                               |
| ------------------- | --------------------------------------------------- |
| NeptuneBackend.Client    | use the "dispatch" functions to send requests       |
| NeptuneBackend.Core      | core funcions, config and request types             |
| NeptuneBackend.API       | construct api requests                              |
| NeptuneBackend.Model     | describes api models                                |
| NeptuneBackend.MimeTypes | encoding/decoding MIME types (content-types/accept) |
| NeptuneBackend.ModelLens | lenses for model fields                             |
| NeptuneBackend.Logging   | logging functions and utils                         |


### MimeTypes

This library adds type safety around what OpenAPI specifies as
Produces and Consumes for each Operation (e.g. the list of MIME types an
Operation can Produce (using 'accept' headers) and Consume (using 'content-type' headers).

For example, if there is an Operation named _addFoo_, there will be a
data type generated named _AddFoo_ (note the capitalization), which
describes additional constraints and actions on the _addFoo_ operation
via its typeclass instances. These typeclass instances can be viewed
in GHCi or via the Haddocks.

* required parameters are included as function arguments to _addFoo_
* optional non-body parameters are included by using  `applyOptionalParam`
* optional body parameters are set by using  `setBodyParam`

Example code generated for pretend _addFoo_ operation: 

```haskell
data AddFoo 	
instance Consumes AddFoo MimeJSON
instance Produces AddFoo MimeJSON
instance Produces AddFoo MimeXML
instance HasBodyParam AddFoo FooModel
instance HasOptionalParam AddFoo FooName
instance HasOptionalParam AddFoo FooId
```

this would indicate that:

* the _addFoo_ operation can consume JSON
* the _addFoo_ operation produces JSON or XML, depending on the argument passed to the dispatch function
* the _addFoo_ operation can set it's body param of _FooModel_ via `setBodyParam`
* the _addFoo_ operation can set 2 different optional parameters via `applyOptionalParam`

If the OpenAPI spec doesn't declare it can accept or produce a certain
MIME type for a given Operation, you should either add a Produces or
Consumes instance for the desired MIME types (assuming the server
supports it), use `dispatchLbsUnsafe` or modify the OpenAPI spec and
run the generator again.

New MIME type instances can be added via MimeType/MimeRender/MimeUnrender

Only JSON instances are generated by default, and in some case
x-www-form-urlencoded instances (FromFrom, ToForm) will also be
generated if the model fields are primitive types, and there are
Operations using x-www-form-urlencoded which use those models.

### Authentication

A haskell data type will be generated for each OpenAPI authentication type.

If for example the AuthMethod `AuthOAuthFoo` is generated for OAuth operations, then
`addAuthMethod` should be used to add the AuthMethod config.

When a request is dispatched, if a matching auth method is found in
the config, it will be applied to the request.

### Example

```haskell
mgr <- newManager defaultManagerSettings
config0 <- withStdoutLogging =<< newConfig 
let config = config0
    `addAuthMethod` AuthOAuthFoo "secret-key"

let addFooRequest = 
  addFoo 
    (ContentType MimeJSON) 
    (Accept MimeXML) 
    (ParamBar paramBar)
    (ParamQux paramQux)
    modelBaz
  `applyOptionalParam` FooId 1
  `applyOptionalParam` FooName "name"
  `setHeader` [("qux_header","xxyy")]
addFooResult <- dispatchMime mgr config addFooRequest
```

See the example app and the haddocks for details.

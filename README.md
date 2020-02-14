# User System

A basic user system using sessions stored in the database and bcrypt.
Useful for branching off of for other projects, kept tearing it out of
old ones so I figured I'd give it a home.

## Module Structure

![Module Structure](mods.png)

## POST /account/change-password

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Samuel Schlesinger (`application/json;charset=utf-8`, `application/json`):

```javascript
{"changePasswordNewPassword":"new-password"}
```

- Samuel Schlesinger (`application/x-www-form-urlencoded`):

```
changePasswordNewPassword=new-password
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/change-username

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Samuel Schlesinger (`application/json;charset=utf-8`, `application/json`):

```javascript
{"changeUsernameNewUsername":"new-username"}
```

- Samuel Schlesinger (`application/x-www-form-urlencoded`):

```
changeUsernameNewUsername=new-username
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/create-object

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Samuel Schlesinger (`application/json;charset=utf-8`, `application/json`):

```javascript
{"createObjectContents":"great new object","createObjectName":"object name"}
```

- Samuel Schlesinger (`application/x-www-form-urlencoded`):

```
createObjectContents=great%20new%20object&createObjectName=object%20name
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/edit-object

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/new-token

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>"),("Access-Control-Allow-Origin",""),("Access-Control-Allow-Headers",""),("Access-Control-Allow-Credentials","false")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/read-object

### Authentication

A simple HTTP-only session based authentication system


Clients must supply the following data
A session cookie


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/signin

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Samuel Schlesinger (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signInUsername":"samuel","signInPassword":"password"}
```

- Samuel Schlesinger (`application/x-www-form-urlencoded`):

```
signInUsername=samuel&signInPassword=password
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","<no header sample provided>"),("Access-Control-Allow-Origin",""),("Access-Control-Allow-Headers",""),("Access-Control-Allow-Credentials","false")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /account/signup

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/x-www-form-urlencoded`

- Samuel Schlesinger (`application/json;charset=utf-8`, `application/json`):

```javascript
{"signUpUsername":"samuel","signUpPassword":"password"}
```

- Samuel Schlesinger (`application/x-www-form-urlencoded`):

```
signUpUsername=samuel&signUpPassword=password
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body



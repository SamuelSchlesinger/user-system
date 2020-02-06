# User System

A basic user system using sessions stored in the database and bcrypt.
Useful for branching off of for other projects, kept tearing it out of
old ones so I figured I'd give it a home.

## Module Structure

![Module Structure](mods.png)

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

## POST /account/signin

### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`

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

    - `application/x-www-form-urlencoded`

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



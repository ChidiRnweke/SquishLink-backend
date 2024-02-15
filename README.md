# URL Shortener Service

## Overview

This Scala-based URL shortener service offers a simple yet effective way to convert long URLs into shorter links. Utilizing a pure functional programming (FP) approach, it ensures robust and efficient operation. Shortened links are generated through a unique combination of an adjective, noun, and number, providing easy-to-remember URLs.

## Key Features

- **Shorten URLs**: Converts long URLs into shorter versions using a memorable pattern.
- **Retrieve Original URLs**: Allows users to access the original URL by visiting the shortened link.
- **Pure Functional Programming**: Built with a focus on pure FP principles for reliability and maintainability.

## How It Works

The service generates short links by combining an adjective, a noun, and a number, creating identifiable, but sometimes goofy URLs. The application interfaces with a database to store and retrieve original URLs based on their shortened counterparts.

## Architecture

The application's architecture is modular, separating concerns into configuration loading, database operations, and HTTP service handling. This structure simplifies maintenance and enhances the clarity of the codebase.

- **Main.scala**: Bootstraps the application and sets up the HTTP server.
- **Environment.scala**: Loads configuration settings from the environment.
- **Shorten.scala**: Contains the logic for URL shortening and database interaction.
- **Shorten.test.scala**: Includes tests to ensure functionality and reliability.

## Getting Started

This project utilizes Scala CLI for building and running, simplifying the setup process. To get started, ensure Scala CLI is installed on your system. Running the application or its tests is straightforward:

To run the service: scala-cli run src/
To execute tests: scala-cli test src/


## Testing

The project includes integration tests that require Docker, as they spin up a database to simulate the full application environment. A Dockerfile is included to facilitate this process, ensuring that you can easily set up a testing environment consistent with the application's requirements.

To run the tests, Docker must be installed and running on your machine. The tests are designed to automatically manage the Docker container setup and teardown, simplifying the process of testing the application in an environment closely resembling production.
# frozen_string_literal: true

require "rack/unreloader"

Unreloader = Rack::Unreloader.new() { BackendServer }
Unreloader.require("backend_server.rb") { "BackendServer" }

run(Unreloader)

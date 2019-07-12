# frozen_string_literal: true

require "rack/unreloader"

Unreloader = Rack::Unreloader.new() { EppServer }
Unreloader.require("epp_server.rb") { "EppServer" }

run(Unreloader)

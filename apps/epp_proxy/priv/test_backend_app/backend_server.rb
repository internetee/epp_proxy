require 'roda'

class BackendServer < Roda
  plugin :render

  route do |r|
    response['Content-Type'] = 'application/xml'

    r.on "session" do
      r.get "hello" do
        render("session/hello")
      end

      r.post "login" do
        render("session/login")
      end

      r.post "logout" do
        render("session/logout")
      end
    end

    r.on "command" do
      r.post "poll" do
        render("command/poll")
      end

    end

    r.on "error" do
    end
  end
end

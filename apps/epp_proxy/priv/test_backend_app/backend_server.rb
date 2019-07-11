require 'roda'

class BackendServer < Roda
  plugin :render

  route do |r|
    response['Content-Type'] = 'application/xml'
    random_number = rand(2)
    @success = random_number == 1

    r.on "session" do
      r.get "hello" do
        render("session/hello")
      end

      r.post "login" do
        if @success
          render("session/login_successful")
        else
          render("session/login_failed")
        end
      end

      r.post "logout" do
      end
    end

    r.on "command" do
      r.post "create" do
      end

    end

    r.on "error" do
    end
  end
end

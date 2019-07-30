require 'roda'

class EppServer < Roda
  plugin :render

  route do |r|
    response['Content-Type'] = 'application/xml'

    r.on "session" do
      r.get "hello" do
        if r.cookies['session']
          render("session/hello")
        end
      end

      r.post "login" do
        render("session/login")
      end

      r.post "logout" do
        # Additional check if passes empty frame
        if r.params['frame']
          render("session/logout")
        end
      end
    end

    r.on "command" do
      r.post "poll" do
        render("command/poll")
      end

    end

    r.get "error" do
      @code = r.params['code']
      @msg = r.params['msg']

      render("error")
    end
  end
end

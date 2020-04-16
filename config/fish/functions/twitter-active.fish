function twitter-active --argument-names username
  echo Checking user $username
  curl -I -s https://twitter.com/$username | head -1 | read _ code
  switch (string trim "$code")
    case 404
      echo Account is free
    case 302
      echo Account is probably suspended
    case 200
      echo Account exists
    case '*'
      echo Unknown status code $code
  end
end

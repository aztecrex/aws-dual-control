module AccessControl.Authorize where


import Data.Monoid ((<>))
import Data.Text (Text, unpack)


class AuthorizeLang r where
    type User r :: *
    type Rule r :: *
    type Account r :: *
    combine :: r (Rule r) -> r (Rule r) -> r (Rule r)
    user :: Text -> r (User r)
    none :: r (Rule r)
    account :: Text -> r (Account r)
    canEmergency :: r (User r) -> r (Account r) -> r (Rule r)
    canAdminister :: r (User r) -> r (Account r) -> r (Rule r)
    canDeploy :: r (User r) -> r (Account r) -> r (Rule r)
    canProvision :: r (User r) -> r (Account r) -> r (Rule r)
    canDual :: r (User r) -> r (Account r) -> r (Rule r)
    canMonitor :: r (User r) -> r (Account r) -> r (Rule r)

authorization :: (AuthorizeLang r) => r (Rule r)
authorization =
    let greg = user "gwiley"
        stu = user "spenrose"
        insightsArea = account "insights-area-prod"
        cas = account "cas-prod"
        automation = account "automation-dev"
        automationEngineers = [user "thekman", user "zyu", user "jlin", user "rwei"]
        accounts = [insightsArea, cas, automation]
        emergency1 = foldl combine none $ map (canEmergency stu) accounts
        emergency2 = foldl combine none $ map (canEmergency greg) accounts
        administer = foldl combine none $ map (canAdminister greg) accounts
        autoDual = foldl combine none $ map (\ac -> canDual ac automation) automationEngineers
        autoMonitor = foldl combine none $ map (\ac -> canDual ac automation) automationEngineers
    in foldl combine none [
        emergency1,
        emergency2,
         administer,
        canEmergency (user "msumme") insightsArea,
        canEmergency (user "thekman") cas,
        autoDual,
        autoMonitor
        ]

putAccessIO :: Text -> IO Text -> IO Text -> IO ()
putAccessIO inter ioUser ioAccount = do
    usr <- ioUser
    act <- ioAccount
    putStrLn $ unpack $ "user '" <> usr <> "' " <> inter <> " '" <> act <> "'"

instance AuthorizeLang IO where
    type User (IO) = Text
    type Rule (IO) = ()
    type Account (IO) = Text
    none = pure ()
    combine = (>>)
    user = pure
    account = pure
    canEmergency = putAccessIO "has emergency access to"
    canAdminister = putAccessIO "can administer"
    canDeploy = putAccessIO "can deploy in"
    canProvision = putAccessIO "can provision in"
    canDual = putAccessIO "has dual control access to"
    canMonitor = putAccessIO "can monitor"


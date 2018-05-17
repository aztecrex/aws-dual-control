module Spec.Permission where

newtype In i = In { runIn :: i -> Bool }

data family Action authority :: *

data instance Action Finance = FinanceAction Text



newtype Actions = Actions (In Action)
newtype Resources = Resources (In Resource)
newtype Principals = Principals (In Principal)

newtype Authority = Authority Text

newtype PrincipalName = PrincipalName Text

data Principal {
    principalAuthority :: Authority,
    principalName :: PrincipalName
}

newtype ResourceName = ResourceName Text

data Resource = Resource {
    resourceAuthority :: Authority,
    resourceName :: Descriptor
}

class Permission r where
    permitted :: Principal -> Action -> Resource -> r Bool

class PermissionLang r where
    type Principals r :: *
    type Actions r :: *
    type Resources r :: *
    permit :: Principals r -> Actions r -> Resources r -> r

data ResourceClass where
    ResourceClass :: (Resource -> Bool) -> ResourceClass

data Statement where
    Allow :: Actions -> ResourceClass -> Conditions -> Statement
    Deny :: Actions -> ResourceClass -> Conditions -> Statement

newtype ActionName = ActionName Text

data Action = Action {
    actionAuthority :: Authority,
    actionName :: ActionName
}

newtype Actions = Actions [Action]

data PrincipalClass where
    PrincipalClass :: (Principal -> Bool) -> PrincipalClass

data Policy where
    Policy :: PrincipalClass -> [Statement] -> Policy

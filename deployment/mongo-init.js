db.createUser(
        {
            user: "diploma",
            pwd: "password",
            roles: [
                {
                    role: "readWrite",
                    db: "diploma"
                }
            ]
        }
);
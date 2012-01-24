(ns leiningen.policy)

(defn policy[project]
	(let [home (System/getProperty "user.home")
		file-seperator (System/getProperty "file.separator")]
	(spit (str home file-seperator ".java.policy") "grant { permission java.security.AllPermission; };")
	(println "Policy created (maybe)")))
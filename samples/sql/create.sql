-- CREATE database awent_samples;


DROP DATABASE IF EXISTS awent_samples;
CREATE DATABASE awent_samples;

USE awent_samples;


CREATE TABLE `books` (
	`id` int(11) NOT NULL auto_increment,
	`author_id` int(11) NULL,
	`title` varchar(100) NOT NULL,
	`extra_info` longtext NOT NULL,
	PRIMARY KEY  (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;



CREATE TABLE `authors` (
	`id` int(11) NOT NULL auto_increment,
	`name` varchar(100) NOT NULL,
	PRIMARY KEY  (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;

